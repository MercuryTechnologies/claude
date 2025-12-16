-- | Error types for Claude API
module Claude.V1.Error
    ( -- * Types
      Error(..)
    , ErrorType(..)
    ) where

import Claude.Prelude

-- | The type of error that occurred
data ErrorType
    = Invalid_Request_Error
    | Authentication_Error
    | Permission_Error
    | Not_Found_Error
    | Rate_Limit_Error
    | Api_Error
    | Overloaded_Error
    deriving stock (Eq, Generic, Show)

errorTypeOptions :: Options
errorTypeOptions = aesonOptions
    { constructorTagModifier = stripPrefix "" }

instance FromJSON ErrorType where
    parseJSON = genericParseJSON errorTypeOptions

instance ToJSON ErrorType where
    toJSON = genericToJSON errorTypeOptions

-- | Error information returned by the Claude API
data Error = Error
    { type_ :: ErrorType
    , message :: Text
    } deriving stock (Generic, Show)

instance FromJSON Error where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Error where
    toJSON = genericToJSON aesonOptions
