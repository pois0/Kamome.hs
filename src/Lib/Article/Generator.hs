{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Article.Generator where

import Control.Concurrent.Async (forConcurrently_)
import Data.List.Split (chunksOf)
import Data.Text (Text, unpack)
import Lib.Article (Article, articleId, readArticle)
import qualified Lib.Article as ARTICLE
import qualified Lib.Article.Meta as META
import qualified Lib.GenerateHTML as GH
import Lib.Utils (createDirectoryIfNotExist)
import System.FilePath.Posix ((<.>), (</>))
import Text.Blaze.Html (preEscapedToMarkup)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamletFile)

data IndexInfo = IndexInfo
  { current :: Int,
    prev :: Maybe Int,
    next :: Maybe Int
  }

indexToFileName :: Int -> String
indexToFileName 1 = "index"
indexToFileName n = show n

slash :: Int -> String
slash 1 = ""
slash _ = "/"

indexToRelUrl :: Int -> String
indexToRelUrl 1 = ""
indexToRelUrl n = show n

articleRootDir :: FilePath -> FilePath
articleRootDir output = output </> "p"

tagRootDir :: FilePath -> FilePath
tagRootDir output = output </> "t"

tagIndicesDir :: FilePath -> Text -> FilePath
tagIndicesDir output tag = tagRootDir output </> unpack tag

generateArticle :: FilePath -> Article -> IO ()
generateArticle output article = do
  md <- readArticle article
  content <- GH.generateHTML md
  writeFile (output ++ articleId article <.> "html") $ renderHtml $(shamletFile "templates/article.hamlet")

generateArticles :: FilePath -> [Article] -> IO ()
generateArticles output articles = do
  let dir = articleRootDir output
  createDirectoryIfNotExist dir
  forConcurrently_ articles $ generateArticle $ dir ++ "/"

generateIndex :: FilePath -> ([Article], IndexInfo) -> IO ()
generateIndex output (articles, IndexInfo {current = i, prev = prev, next = next}) = do
  writeFile (output ++ indexToFileName i <.> "html") $ renderHtml $(shamletFile "templates/index.hamlet")

generateIndices :: Int -> FilePath -> [Article] -> IO ()
generateIndices n output articles = forConcurrently_ (toIndexInfo $ chunksOf n articles) $ generateIndex $ output ++ "/"

generateTagIndex :: FilePath -> Text -> ([Article], IndexInfo) -> IO ()
generateTagIndex output tag (articles, IndexInfo {current = i, prev = prev, next = next}) = do
  writeFile (tagIndicesDir output tag </> indexToFileName i <.> "html") $ renderHtml $(shamletFile "templates/tags.hamlet")

generateTagIndices :: Int -> FilePath -> [(Text, [Article])] -> IO ()
generateTagIndices n output list = do
  let dir = tagRootDir output
  createDirectoryIfNotExist dir
  forConcurrently_ list tagHandler
  where
    tagHandler :: (Text, [Article]) -> IO ()
    tagHandler (tag, articles) = do
      createDirectoryIfNotExist $ tagIndicesDir output tag
      forConcurrently_ (toIndexInfo $ chunksOf n articles) $ generateTagIndex output tag

toIndexInfo :: [[Article]] -> [([Article], IndexInfo)]
toIndexInfo [] = []
toIndexInfo [x] = [(x, IndexInfo 1 Nothing Nothing)]
toIndexInfo (x : xs) = (x, IndexInfo 1 (Just 2) Nothing) : toIndexInfo_ 2 xs
  where
    toIndexInfo_ :: Int -> [[Article]] -> [([Article], IndexInfo)]
    toIndexInfo_ _ [] = []
    toIndexInfo_ i [y] = [(y, IndexInfo i Nothing (Just $ i - 1))]
    toIndexInfo_ i (y : ys) = (y, IndexInfo i (Just $ i + 1) (Just $ i - 1)) : toIndexInfo_ (i + 1) ys
