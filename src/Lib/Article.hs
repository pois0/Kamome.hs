{-# LANGUAGE TupleSections #-}

module Lib.Article where

import qualified Data.List as L 
import Data.List.Extra (groupSort)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text.IO as TIO 
import Data.Yaml (ParseException)
import qualified Lib.Article.Meta as M
import Lib.Article.Meta (Meta)
import qualified Lib.GenerateHTML as GH
import System.Directory (listDirectory)

data Article = Article
  { dir :: FilePath
  , meta :: Meta
  }
  
data ArticleLoadFailedException = ArticleLoadFailedException FilePath ParseException

articleId :: Article -> String
articleId = last . (splitOn "/" . dir)

metaPos :: FilePath
metaPos = "/meta.yaml"

metaPath :: Article -> FilePath
metaPath article = dir article ++ metaPos

contentPath :: Article -> FilePath
contentPath article = dir article ++ "/content.md"

outputPath :: Article -> FilePath -> FilePath
outputPath article path = path ++ "/" ++ articleId article ++ ".html"

loadArticle :: FilePath -> IO (Either ParseException Article)
loadArticle path = do
  meta <- M.loadMeta $ path ++ metaPos
  return $ fmap (Article path) meta
  
getArticles :: FilePath -> IO [Either ArticleLoadFailedException Article]
getArticles path = do
  paths <- listDirectory path
  mapM (\ path -> either (Left . ArticleLoadFailedException path) Right <$> loadArticle path) paths

readArticle :: Article -> IO Text
readArticle = TIO.readFile . contentPath 

sortByCreatedAt :: [Article] -> [Article]
sortByCreatedAt = L.sortOn $ M.createdAt . meta

classifyByTag :: [Article] -> [(Text, [Article])]
classifyByTag = groupSort . concatMap (\ ar -> map (, ar) $ (M.tags . meta) ar)

generateHTML :: FilePath -> Article  -> IO ()
generateHTML output article = do
  md <- readArticle article
  html <- GH.generateHTML md
  TIO.writeFile (outputPath article output) html
