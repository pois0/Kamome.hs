module Lib.Utils where

import System.Directory (createDirectory, doesDirectoryExist)

createDirectoryIfNotExist :: FilePath -> IO ()
createDirectoryIfNotExist dir = do
  exist <- doesDirectoryExist dir
  if not exist then createDirectory dir else mempty
