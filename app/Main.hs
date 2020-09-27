module Main where

import Control.Concurrent.Async (forConcurrently_)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Lib.Article (Article, classifyByTag, exception, filterByVisibility, sortByCreatedAt)
import qualified Lib.Article as A
import Lib.Article.Generator (generateArticles, generateIndices, generateTagIndices)
import Lib.Config (Config (..), loadConfig, repositoryPath)
import Relude.List ((!!?))
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)

config :: IO Config
config = do
  args <- getArgs
  conf <- loadConfig $ fromMaybe "config.yaml" $ args !!? 1
  either (\_ -> die "missing config.yaml") return conf

getArticles :: FilePath -> IO [Article]
getArticles path = do
  dirs <- A.getArticles path
  let (exceptions, articles) = partitionEithers dirs
  if null exceptions then return $ sortByCreatedAt $ filterByVisibility articles else printMessage exceptions
  where
    printMessage :: [A.ArticleLoadFailedException] -> IO a
    printMessage es = do
      print $ exception $ head es
      die ""

checkDir :: FilePath -> IO ()
checkDir path = do
  exist <- doesDirectoryExist path
  if not exist then createDirectory path else mempty

main :: IO ()
main = do
  conf <- config
  articles <- getArticles . repositoryPath $ conf
  checkDir $ outputPath conf
  forConcurrently_
    [ generateArticles (outputPath conf),
      generateIndices (articlesPerPage conf) (outputPath conf),
      generateTagIndices (articlesPerPage conf) (outputPath conf) . classifyByTag
    ]
    (\op -> op articles)
