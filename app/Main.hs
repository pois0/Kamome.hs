module Main where

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Yaml (ParseException)
import qualified Lib.Article as A
import Lib.Article (Article)
import qualified Lib.Config as C
import Lib.Config (Config, repositoryPath)
import Relude.List ((!!?))
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
  if null exceptions then printMessage exceptions else return articles
  where
    printMessage :: [A.ArticleLoadFailedException] -> IO a
    printMessage es = die ""
    
generateArticles :: FilePath -> [Article] -> IO ()
generateArticles path = mapM_ $ A.generateHTML path 

main :: IO ()
main = do
  conf <- loadConfig
  articles <- getArticles . C.repositoryPath $ conf
  generateArticles (C.outputPath conf) articles
  mempty
