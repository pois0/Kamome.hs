module Main where

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Yaml (ParseException)
import qualified Lib.Article as A
import Lib.Article.Generator (generateArticles, generateIndices, generateTagIndices)
import Lib.Article (Article, exception, sortByCreatedAt, classifyByTag)
import qualified Lib.Config as C
import Lib.Config (Config, repositoryPath)
import Relude.List ((!!?))
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die)

loadConfig :: IO Config
loadConfig = do
  args <- getArgs
  conf <- C.loadConfig $ fromMaybe "config.yaml" $ args !!? 1
  either (\ _ -> die "missing config.yaml") return conf

getArticles :: FilePath -> IO [Article]
getArticles path = do
  dirs <- A.getArticles path
  let (exceptions, articles) = partitionEithers dirs
  if null exceptions then return $ sortByCreatedAt articles else printMessage exceptions
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
  conf <- loadConfig
  articles <- getArticles . C.repositoryPath $ conf
  checkDir $ C.outputPath conf
  generateArticles (C.outputPath conf) articles
  generateIndices (C.articlesPerPage conf) (C.outputPath conf) articles
  generateTagIndices (C.articlesPerPage conf) (C.outputPath conf) $ classifyByTag articles
  mempty
