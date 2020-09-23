{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Lib.Article.Generator where

import Data.List.Split (chunksOf)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import qualified Lib.Article as ARTICLE
import Lib.Article (Article, readArticle, articleId)
import qualified Lib.Article.Meta as META
import qualified Lib.GenerateHTML as GH
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath.Posix ((</>), (<.>))
import Text.Blaze.Html (preEscapedToMarkup)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamletFile)

data IndexInfo = IndexInfo
  { current :: Int
  , prev :: Maybe Int
  , next :: Maybe Int
  }

indexToFileName :: Int -> String
indexToFileName 1 = "index"
indexToFileName n = show n

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
  exist <- doesDirectoryExist dir
  if not exist then createDirectory dir else mempty
  mapM_ (generateArticle $ dir ++ "/") articles 

generateIndex :: FilePath -> ([Article], IndexInfo) -> IO ()
generateIndex output (articles, IndexInfo {current = i, prev = prev, next = next}) = do
  writeFile (output ++ indexToFileName i <.> "html") $ renderHtml $(shamletFile "templates/index.hamlet")

generateIndices :: Int -> FilePath -> [Article] -> IO ()
generateIndices n output articles = mapM_ (generateIndex $ output ++ "/") $ toIndexInfo $ chunksOf n articles

generateTagIndex :: FilePath -> Text -> ([Article], IndexInfo) -> IO ()
generateTagIndex output tag (articles, IndexInfo {current = i, prev = prev, next = next}) = do
  writeFile (tagIndicesDir output tag </> indexToFileName i <.> "html") $ renderHtml $(shamletFile "templates/tags.hamlet")

generateTagIndices :: Int -> FilePath -> [(Text, [Article])] -> IO ()
generateTagIndices n output list = do
  let dir = tagRootDir output
  exist <- doesDirectoryExist dir
  if not exist then createDirectory dir else mempty
  mapM_ tagHandler list
  where
    tagHandler :: (Text, [Article]) -> IO ()
    tagHandler (tag, articles) = do
      exist <- doesDirectoryExist $ tagIndicesDir output tag
      if not exist then createDirectory $ tagIndicesDir output tag else mempty
      mapM_ (generateTagIndex output tag) $ toIndexInfo $ chunksOf n articles

toIndexInfo :: [[Article]] -> [([Article], IndexInfo)]
toIndexInfo [] = []
toIndexInfo [x] = [(x, IndexInfo 1 Nothing Nothing)]
toIndexInfo (x:xs) = (x, IndexInfo 1 (Just 2) Nothing) : toIndexInfo_ 2 xs
  where
    toIndexInfo_ :: Int -> [[Article]] -> [([Article], IndexInfo)]
    toIndexInfo_ _ [] = []
    toIndexInfo_ i [x] = [(x, IndexInfo i Nothing (Just $ i - 1))]
    toIndexInfo_ i (x:xs) = (x, IndexInfo i (Just $ i + 1) (Just $ i - 1)) : toIndexInfo_ (i + 1) xs
