module Main where

import System.Environment
import FromXlsx
import StringsFiles
import MergeXlsxToXml
import Text.XML.HXT.Core

main = do
  xlsxFn:_ <- getArgs
  bm <- bigMapFromFile xlsxFn
  runIOLA ((IOLA (const valuesDirs)) >>> (mergeXlsxToXml bm)) undefined
  return ()
