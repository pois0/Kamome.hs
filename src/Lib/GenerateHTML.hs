module Lib.GenerateHTML
  ( generateHTML
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Text.Blaze.Html (Html)
import Text.Pandoc

generateHTML :: Text -> IO Html
generateHTML md = do
  result <- runIO $ do
    doc <- readCommonMark def md
    writeHtml5 def doc
  handleError result
