module Claude.Prelude
    ( -- * JSON
      aesonOptions
    , stripPrefix
    , labelModifier
      -- * Re-exports
    , module Data.Aeson
    , module Data.ByteString.Lazy
    , module Data.Map
    , module Data.String
    , module Data.Text
    , module Data.Time.Clock.POSIX
    , module Data.Vector
    , module Data.Void
    , module GHC.Generics
    , module Numeric.Natural
    , module Servant.API
    , module Web.HttpApiData
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Web.HttpApiData (ToHttpApiData(..))

import Data.Aeson
    ( FromJSON(..)
    , Options(..)
    , SumEncoding(..)
    , ToJSON(..)
    , Value(..)
    , genericParseJSON
    , genericToJSON
    )
import Servant.API
    ( Accept(..)
    , Capture
    , Delete
    , Get
    , Header'
    , JSON
    , MimeUnrender(..)
    , OctetStream
    , Optional
    , Post
    , QueryParam
    , ReqBody
    , Required
    , Strict
    , (:<|>)(..)
    , (:>)
    )

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.List as List

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore "_" = ""
dropTrailingUnderscore ""  = ""
dropTrailingUnderscore (c : cs) = c : dropTrailingUnderscore cs

labelModifier :: String -> String
labelModifier = map Char.toLower . dropTrailingUnderscore

stripPrefix :: String -> String -> String
stripPrefix prefix string = labelModifier suffix
  where
    suffix = case List.stripPrefix prefix string of
        Nothing -> string
        Just x  -> x

aesonOptions :: Options
aesonOptions = Aeson.defaultOptions
    { fieldLabelModifier = labelModifier
    , constructorTagModifier = labelModifier
    , omitNothingFields = True
    }
