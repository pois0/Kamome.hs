{-# LANGUAGE TupleSections #-}

module Lib.Article where

import qualified Data.List as L
import Data.List.Extra (groupSort)
import Data.List.Split (splitOn)
import qualified Data.Ord as Ord
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Yaml (ParseException)
import Lib.Article.Meta (Meta)
import qualified Lib.Article.Meta as M
import System.Directory (listDirectory)
import System.FilePath.Posix ((<.>), (</>))

data Article = Article
  { dir :: FilePath,
    meta :: Meta
  }

data ArticleLoadFailedException = ArticleLoadFailedException
  { path :: FilePath,
    exception :: ParseException
  }

articleId :: Article -> String
articleId = last . (splitOn "/" . dir)

metaPath :: FilePath -> FilePath
metaPath = flip (</>) ("meta" <.> "yaml")

contentPath :: Article -> FilePath
contentPath article = dir article </> "content" <.> "md"

loadArticle :: FilePath -> IO (Either ParseException Article)
loadArticle p = fmap (fmap $ Article p) $ M.loadMeta . metaPath $ p

getArticles :: FilePath -> IO [Either ArticleLoadFailedException Article]
getArticles repositoryPath = do
  paths <- listDirectory repositoryPath
  mapM (\p -> either (Left . ArticleLoadFailedException p) Right <$> loadArticle (repositoryPath ++ p)) paths

readArticle :: Article -> IO Text
readArticle = TIO.readFile . contentPath

sortByCreatedAt :: [Article] -> [Article]
sortByCreatedAt = L.sortOn $ Ord.Down . M.createdAt . meta

classifyByTag :: [Article] -> [(Text, [Article])]
classifyByTag = groupSort . concatMap (\ar -> map (,ar) $ (M.tags . meta) ar)
