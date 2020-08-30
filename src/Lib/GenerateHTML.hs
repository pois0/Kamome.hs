module Lib.GenerateHTML
  ( generateHTML
  ) where

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

generateHTML :: Text -> IO Text
generateHTML md = do
  result <- runIO $ do
    doc <- readMarkdown def md
    writeHtml5String def doc
  handleError result
  