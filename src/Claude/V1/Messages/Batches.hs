-- | @\/v1\/messages\/batches@
--
-- This module provides types for the Claude Message Batches API.
-- Batches allow asynchronous processing of multiple messages at once.
module Claude.V1.Messages.Batches
    ( -- * Request types
      CreateBatch(..)
    , _CreateBatch
    , BatchRequest(..)
      -- * Response types
    , BatchObject(..)
    , ProcessingStatus(..)
    , RequestCounts(..)
    , BatchResult(..)
    , BatchResultType(..)
    , ListBatchesResponse(..)
      -- * Servant
    , API
    ) where

import Claude.Prelude
import Claude.V1.Messages (CreateMessage, MessageResponse)

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Claude.V1.Error as Error

-- | A single request within a batch
data BatchRequest = BatchRequest
    { custom_id :: Text    -- ^ Developer-provided ID for matching results
    , params :: CreateMessage  -- ^ The message request parameters
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for creating a batch
data CreateBatch = CreateBatch
    { requests :: Vector BatchRequest
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default CreateBatch
_CreateBatch :: CreateBatch
_CreateBatch = CreateBatch
    { requests = mempty
    }

-- | Processing status of a batch
data ProcessingStatus
    = In_Progress
    | Canceling
    | Ended
    deriving stock (Eq, Generic, Show)

processingStatusOptions :: Options
processingStatusOptions = Aeson.defaultOptions
    { constructorTagModifier = map Char.toLower
    }

instance FromJSON ProcessingStatus where
    parseJSON = genericParseJSON processingStatusOptions

instance ToJSON ProcessingStatus where
    toJSON = genericToJSON processingStatusOptions

-- | Counts of requests in various states
data RequestCounts = RequestCounts
    { processing :: Natural
    , succeeded :: Natural
    , errored :: Natural
    , canceled :: Natural
    , expired :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A batch object returned from the API
data BatchObject = BatchObject
    { id :: Text
    , type_ :: Text                    -- ^ Always "message_batch"
    , processing_status :: ProcessingStatus
    , request_counts :: RequestCounts
    , ended_at :: Maybe POSIXTime
    , created_at :: POSIXTime
    , expires_at :: POSIXTime
    , cancel_initiated_at :: Maybe POSIXTime
    , results_url :: Maybe Text
    } deriving stock (Generic, Show)

instance FromJSON BatchObject where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON BatchObject where
    toJSON = genericToJSON aesonOptions

-- | Type of result for a batch request
data BatchResultType
    = Succeeded { message :: MessageResponse }
    | Errored { error :: Error.Error }
    | Canceled
    | Expired
    deriving stock (Generic, Show)

batchResultTypeOptions :: Options
batchResultTypeOptions = Aeson.defaultOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = map Char.toLower
    }

instance FromJSON BatchResultType where
    parseJSON = genericParseJSON batchResultTypeOptions

instance ToJSON BatchResultType where
    toJSON = genericToJSON batchResultTypeOptions

-- | A single result from a batch
data BatchResult = BatchResult
    { custom_id :: Text
    , result :: BatchResultType
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Pagination response for listing batches
data ListBatchesResponse = ListBatchesResponse
    { data_ :: Vector BatchObject
    , has_more :: Bool
    , first_id :: Maybe Text
    , last_id :: Maybe Text
    } deriving stock (Generic, Show)

instance FromJSON ListBatchesResponse where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ListBatchesResponse where
    toJSON = genericToJSON aesonOptions

-- | Servant API for @\/v1\/messages\/batches@
type API =
        "messages"
    :>  "batches"
    :>  (         ReqBody '[JSON] CreateBatch
            :>  Post '[JSON] BatchObject
        :<|>      Capture "batch_id" Text
            :>  Get '[JSON] BatchObject
        :<|>      QueryParam "limit" Natural
            :>  QueryParam "before_id" Text
            :>  QueryParam "after_id" Text
            :>  Get '[JSON] ListBatchesResponse
        :<|>      Capture "batch_id" Text
            :>  "cancel"
            :>  Post '[JSON] BatchObject
        )
