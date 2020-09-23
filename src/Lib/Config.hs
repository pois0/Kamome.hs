{-# LANGUAGE OverloadedStrings #-}

module Lib.Config
 ( Config(..)
 , loadConfig
 )where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON, (.:), ParseException)

data Config = Config
  { repositoryPath :: FilePath
  , outputPath :: FilePath
  , articlesPerPage :: Int
  }

instance FromJSON Config where
  parseJSON (Y.Object v) = Config
    <$> v .: "repository_path"
    <*> v .: "output_path"
    <*> v .: "articles_per_page"
  parseJSON _ = fail "Unexpected object for Config value"
  
loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = Y.decodeFileEither
