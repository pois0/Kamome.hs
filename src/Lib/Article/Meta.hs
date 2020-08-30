{-# LANGUAGE OverloadedStrings #-}

module Lib.Article.Meta 
  ( Meta(..)
  , loadMeta
  )where

import Data.Text (Text)
import Data.Time (Day)
import Data.Semigroup (Option)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON, (.:), ParseException)
import Lib.Config (Config)

data Meta = Meta
  { title :: Text
  , tags :: [Text]
  , createdAt :: Day
  , modifiedAt :: Option Day
  }
  
instance FromJSON Meta where
  parseJSON (Y.Object v) = Meta
    <$> v .: "title"
    <*> v .: "tags"
    <*> v .: "created_at"
    <*> v .: "modified_at" 
  parseJSON _ = fail "Unexpected object for Config value"

loadMeta :: FilePath -> IO (Either ParseException Meta)
loadMeta = Y.decodeFileEither