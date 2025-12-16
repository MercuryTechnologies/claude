-- | @\/v1@
--
-- Example usage:
--
-- @
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE NamedFieldPuns        #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE OverloadedLists       #-}
--
-- module Main where
--
-- import "Data.Foldable" (`Data.Foldable.traverse_`)
-- import "Claude.V1"
-- import "Claude.V1.Messages"
--
-- import qualified "Data.Text" as Text
-- import qualified "Data.Text.IO" as Text.IO
-- import qualified "System.Environment" as Environment
--
-- main :: `IO` ()
-- main = do
--     key <- Environment.`System.Environment.getEnv` \"ANTHROPIC_KEY\"
--
--     clientEnv <- `Claude.V1.getClientEnv` \"https://api.anthropic.com\"
--
--     let `Claude.V1.Methods`{ createMessage } = `Claude.V1.makeMethods` clientEnv (Text.`Data.Text.pack` key) (Just \"2023-06-01\")
--
--     text <- Text.IO.`Data.Text.IO.getLine`
--
--     `Claude.V1.Messages.MessageResponse`{ `Claude.V1.Messages.content` } <- createMessage `Claude.V1.Messages._CreateMessage`
--         { `Claude.V1.Messages.model` = \"claude-sonnet-4-20250514\"
--         , `Claude.V1.Messages.messages` =
--             [ `Claude.V1.Messages.Message`
--                 { `Claude.V1.Messages.role` = `Claude.V1.Messages.User`
--                 , `Claude.V1.Messages.content` = [ `Claude.V1.Messages.Content_Text`{ `Claude.V1.Messages.text` } ]
--                 }
--             ]
--         , `Claude.V1.Messages.max_tokens` = 1024
--         }
--
--     let display (`Claude.V1.Messages.ContentBlock_Text`{ `Claude.V1.Messages.text` = t }) = Text.IO.`Data.Text.IO.putStrLn` t
--         display _ = pure ()
--
--     `Data.Foldable.traverse_` display content
-- @

module Claude.V1
    ( -- * Methods
      Methods(..)
    , getClientEnv
    , makeMethods
      -- * Servant
    , API
    ) where

import Claude.Prelude
import Claude.V1.Messages (CountTokensRequest, CreateMessage, MessageResponse, MessageStreamEvent, TokenCount)
import Claude.V1.Messages.Batches (BatchObject, CreateBatch, ListBatchesResponse)
import Control.Monad (foldM)
import Data.ByteString.Char8 ()
import Data.Proxy (Proxy(..))
import Servant.Client (ClientEnv)

import qualified Claude.V1.Messages as Messages
import qualified Claude.V1.Messages.Batches as Batches
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as S8
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Int
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import qualified Servant.Client as Client

-- | Convenient utility to get a `ClientEnv` for the most common use case
getClientEnv
    :: Text
    -- ^ Base URL for API (e.g., "https://api.anthropic.com")
    -> IO ClientEnv
getClientEnv baseUrlText = do
    baseUrl <- Client.parseBaseUrl (Text.unpack baseUrlText)

    let managerSettings = TLS.tlsManagerSettings
            { HTTP.Client.managerResponseTimeout =
                HTTP.Client.responseTimeoutNone
            }

    manager <- TLS.newTlsManagerWith managerSettings

    pure (Client.mkClientEnv manager baseUrl)

-- | Get a record of API methods after providing an API key
makeMethods
    :: ClientEnv
    -- ^
    -> Text
    -- ^ API key
    -> Maybe Text
    -- ^ Anthropic-Version header (e.g., "2023-06-01")
    -> Methods
makeMethods clientEnv apiKey version = Methods{..}
  where
    ((createMessage_ :<|> countTokens_) :<|> (createBatch_ :<|> retrieveBatch_ :<|> listBatches_ :<|> cancelBatch_)) =
        Client.hoistClient @API Proxy run (Client.client @API Proxy) apiKey version

    run :: Client.ClientM a -> IO a
    run clientM = do
        result <- Client.runClientM clientM clientEnv
        case result of
            Left exception -> Exception.throwIO exception
            Right a -> return a

    createMessage = createMessage_
    countTokens = countTokens_
    createBatch = createBatch_
    retrieveBatch = retrieveBatch_
    listBatches = listBatches_
    cancelBatch = cancelBatch_

    -- Streaming implementation using http-client and SSE parsing
    createMessageStream req onEvent = do
        let req' = req{ Messages.stream = Just True }
        ssePostJSON "/v1/messages" req' onEvent

    createMessageStreamTyped
        :: CreateMessage
        -> (Either Text MessageStreamEvent -> IO ())
        -> IO ()
    createMessageStreamTyped req onEvent =
        createMessageStream req $ \ev -> case ev of
            Left err -> onEvent (Left err)
            Right val -> case Aeson.fromJSON val of
                Aeson.Error msg -> onEvent (Left (Text.pack msg))
                Aeson.Success e -> onEvent (Right e)

    ssePostJSON :: ToJSON a
                => String
                -> a
                -> (Either Text Aeson.Value -> IO ())
                -> IO ()
    ssePostJSON path body onEvent = do
        let base = Client.baseUrl clientEnv
        let secure = case Client.baseUrlScheme base of
                Client.Http -> False
                Client.Https -> True
        let host = S8.pack (Client.baseUrlHost base)
        let port = Client.baseUrlPort base
        let basePath = Client.baseUrlPath base
        let fullPath = S8.pack (normalizePath basePath <> path)

        let headers0 =
                [ ("x-api-key", S8.pack (Text.unpack apiKey))
                , ("Accept", "text/event-stream")
                , ("Content-Type", "application/json")
                ]
        let headers = case version of
                Nothing -> headers0
                Just v -> ("anthropic-version", S8.pack (Text.unpack v)) : headers0

        let request = HTTP.Client.defaultRequest
                { HTTP.Client.secure = secure
                , HTTP.Client.host = host
                , HTTP.Client.port = port
                , HTTP.Client.method = "POST"
                , HTTP.Client.path = fullPath
                , HTTP.Client.requestHeaders = headers
                , HTTP.Client.requestBody = HTTP.Client.RequestBodyLBS (Aeson.encode body)
                , HTTP.Client.responseTimeout = HTTP.Client.responseTimeoutNone
                }

        HTTP.Client.withResponse request (Client.manager clientEnv) $ \response -> do
            -- Short-circuit on non-2xx HTTP statuses and surface a single error event
            let st = HTTP.Client.responseStatus response
            if not (Status.statusIsSuccessful st)
                then do
                    bodyChunks <- HTTP.Client.brConsume (HTTP.Client.responseBody response)
                    let errBody = SBS.concat bodyChunks
                    let msg =
                            "HTTP error "
                            <> renderIntegral (Status.statusCode st)
                            <> " "
                            <> (Text.pack (S8.unpack (Status.statusMessage st)))
                            <> (if SBS.null errBody then "" else ": " <> Text.pack (S8.unpack errBody))
                    onEvent (Left msg)
                else do
                    let br = HTTP.Client.responseBody response
                    lineBufRef <- IORef.newIORef SBS.empty
                    eventBufRef <- IORef.newIORef ([] :: [SBS.ByteString])
                    let flushEvent = do
                            es <- IORef.atomicModifyIORef eventBufRef (\buf -> ([], reverse buf))
                            if null es
                                then pure False
                                else do
                                    let payload = S8.concat es
                                    -- Claude uses "event: message_stop" as the final event, not [DONE]
                                    case (Aeson.eitherDecodeStrict payload :: Either String Aeson.Value) of
                                        Left err -> onEvent (Left (Text.pack err)) >> pure False
                                        Right val -> onEvent (Right val) >> pure False

                    -- Note: SSE frames can include fields like "event:" and others.
                    -- We currently only buffer "data:" lines; an empty line flushes a complete event.
                    let handleLine line = do
                            let l = stripCR line
                            if S8.null l
                                then flushEvent
                                else if "data:" `S8.isPrefixOf` l
                                    then do
                                        let d = S8.dropWhile (==' ') (S8.drop 5 l)
                                        IORef.modifyIORef' eventBufRef (d:)
                                        pure False
                                    else pure False

                    let loop = do
                            chunk <- HTTP.Client.brRead br
                            if SBS.null chunk
                                then do
                                    -- flush any pending event at EOF
                                    _ <- flushEvent
                                    pure ()
                                else do
                                    prev <- IORef.readIORef lineBufRef
                                    let combined = prev <> chunk
                                    let ls = S8.split '\n' combined
                                    case unsnoc ls of
                                        Nothing -> loop
                                        Just (completeLines, lastLine) -> do
                                            IORef.writeIORef lineBufRef lastLine
                                            stop <- foldM (\acc ln -> if acc then pure True else handleLine ln) False completeLines
                                            if stop then pure () else loop

                    loop

    normalizePath p = case p of
        "" -> ""
        ('/':_) -> p
        _ -> '/':p

    stripCR bs = case S8.unsnoc bs of
        Just (initBs, '\r') -> initBs
        _ -> bs

    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)

    renderIntegral :: Integral number => number -> Text
    renderIntegral number = Text.Lazy.toStrict (Builder.toLazyText builder)
      where
        builder = Int.decimal number

-- | API methods
data Methods = Methods
    { createMessage :: CreateMessage -> IO MessageResponse
    , createMessageStream
        :: CreateMessage
        -> (Either Text Aeson.Value -> IO ())
        -> IO ()
    , createMessageStreamTyped
        :: CreateMessage
        -> (Either Text MessageStreamEvent -> IO ())
        -> IO ()
    , countTokens :: CountTokensRequest -> IO TokenCount
      -- Batch methods
    , createBatch :: CreateBatch -> IO BatchObject
    , retrieveBatch :: Text -> IO BatchObject
    , listBatches
        :: Maybe Natural  -- ^ limit
        -> Maybe Text     -- ^ before_id
        -> Maybe Text     -- ^ after_id
        -> IO ListBatchesResponse
    , cancelBatch :: Text -> IO BatchObject
    }

-- | Servant API
type API
    =   Header' [ Required, Strict ] "x-api-key" Text
    :>  Header' [ Optional, Strict ] "anthropic-version" Text
    :>  "v1"
    :>  (Messages.API :<|> Batches.API)
