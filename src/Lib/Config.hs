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
  }

instance FromJSON Config where
  parseJSON (Y.Object v) = Config
    <$> v .: "repository_path"
    <*> v .: "output_path" 
  parseJSON _ = fail "Unexpected object for Config value"
  
loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = Y.decodeFileEither
