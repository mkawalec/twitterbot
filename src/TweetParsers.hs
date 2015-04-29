{-# LANGUAGE CPP #-}

module TweetParsers where

import              Data.Aeson.Types
import              Data.Time.Format (readTime, parseTime)
import              Control.Applicative
import              Data.Time.LocalTime (LocalTime)
import              Control.Monad
import              Data.Text
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import              System.Locale (defaultTimeLocale)
#endif
import              Debug.Trace

data TweetUrl = TweetUrl { expandedUrl :: String
                         , url :: String
                         , indices :: [Integer]
                         , displayUrl :: String
                         } deriving (Eq, Show)

data TweetEntities = TweetEntities { urls :: [TweetUrl]
                                } deriving (Eq, Show)

data TimelineEntry = TimelineEntry { favorited :: Bool
                                   , truncated :: Bool
                                   , createdAt :: LocalTime
                                   , idStr :: String
                                   , entities :: !TweetEntities
                                   , text :: !String
                                   } deriving (Eq, Show)

instance FromJSON TweetEntities where
    parseJSON (Object v) = TweetEntities <$> v .: "urls"
    parseJSON _          = mzero

instance FromJSON TweetUrl where
    parseJSON (Object v) = TweetUrl <$>
                           v .: "expanded_url" <*>
                           v .: "url" <*>
                           v .: "indices" <*>
                           v .: "display_url"
    parseJSON _          = mzero

instance FromJSON LocalTime where
    parseJSON = withText "LocalTime" $ \t -> 
        case parseTime defaultTimeLocale "%a %b %d %T %z %Y" (unpack t) of
            Just d -> pure d
            _      -> fail "coudn't parse twitter date"

instance FromJSON TimelineEntry where
    parseJSON (Object v) = TimelineEntry <$>
                           v .: "favorited" <*>
                           v .: "truncated" <*>
                           v .: "created_at" <*>
                           v .: "id_str" <*>
                           v .: "entities" <*>
                           v .: "text"
    parseJSON _          = mzero

type Timeline = [TimelineEntry]
