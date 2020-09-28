{-# LANGUAGE OverloadedStrings #-}

module Lib.GenerateHTML
  ( generateHTML,
  )
where

import Data.Default ()
import Data.Text (Text)
import Text.Blaze.Html (Html)
import Text.Pandoc

readerOption :: ReaderOptions
readerOption = def {readerExtensions = getDefaultExtensions "commonmark_x"}

writerOption :: WriterOptions
writerOption =
  def
    { writerIdentifierPrefix = "ref:",
      writerExtensions = getDefaultExtensions "commonmark_x",
      writerReferenceLinks = True
    }

generateHTML :: Text -> IO Html
generateHTML md = do
  result <- runIO $ do
    doc <- readCommonMark readerOption md
    writeHtml5 writerOption doc
  handleError result
