module Claude.V1.CacheControl
    ( CacheControl(..)
    , CacheTTL(..)
    , ephemeralCache
    , ephemeralCacheWithTTL
    , ephemeralCacheWithTTLSeconds
    , ephemeralCacheWithTTLDuration
    ) where

import Claude.Prelude
import qualified Data.Aeson as Aeson

-- | Cache TTL configuration for prompt caching.
--
-- Supports both numeric seconds and duration strings for forward compatibility.
data CacheTTL
    = CacheTTLSeconds Natural
    | CacheTTLDuration Text
    deriving stock (Eq, Show)

instance FromJSON CacheTTL where
    parseJSON n@(Aeson.Number _) = CacheTTLSeconds <$> Aeson.parseJSON n
    parseJSON (Aeson.String t) = pure (CacheTTLDuration t)
    parseJSON _ = fail "CacheTTL must be a number (seconds) or string duration"

instance ToJSON CacheTTL where
    toJSON (CacheTTLSeconds n) = Aeson.toJSON n
    toJSON (CacheTTLDuration t) = Aeson.toJSON t

-- | Cache control for prompt caching.
data CacheControl = CacheControl
    { type_ :: Text
    , ttl :: Maybe CacheTTL
    } deriving stock (Eq, Generic, Show)

instance FromJSON CacheControl where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CacheControl where
    toJSON = genericToJSON aesonOptions

-- | Convenience constructor for ephemeral cache control.
ephemeralCache :: CacheControl
ephemeralCache = CacheControl
    { type_ = "ephemeral"
    , ttl = Nothing
    }

-- | Convenience constructor for ephemeral cache control with TTL.
ephemeralCacheWithTTL :: CacheTTL -> CacheControl
ephemeralCacheWithTTL cacheTTL = CacheControl
    { type_ = "ephemeral"
    , ttl = Just cacheTTL
    }

-- | Convenience constructor for ephemeral cache control with TTL in seconds.
ephemeralCacheWithTTLSeconds :: Natural -> CacheControl
ephemeralCacheWithTTLSeconds = ephemeralCacheWithTTL . CacheTTLSeconds

-- | Convenience constructor for ephemeral cache control with TTL duration text.
ephemeralCacheWithTTLDuration :: Text -> CacheControl
ephemeralCacheWithTTLDuration = ephemeralCacheWithTTL . CacheTTLDuration
