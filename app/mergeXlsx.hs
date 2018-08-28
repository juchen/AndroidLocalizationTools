module Main where

import System.Environment
import FromXlsx
import StringsFiles
import MergeXlsxToXml
import Text.XML.HXT.Core

main = do
  args <- getArgs
  if args == []
    then
      putStrLn "\nUsage: merge_xlsx <xlsx file>\n\n\tUpdate all strings.xml file according to the specified xlsx file.\n\tNeed to be run in the src/main/res/ directory.\n"
    else do
      bm <- bigMapFromFile (head args)
      runIOLA ((IOLA (const valuesDirs)) >>> (mergeXlsxToXml bm)) undefined
      return ()
