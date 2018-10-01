module Main where

import System.Environment
import System.IO
import FromXlsx
import StringsFiles
import MergeXlsxToXml
import Text.XML.HXT.Core

expArrow:: IOStateArrow s b String
expArrow = readDocument [] "/tmp/tmp.xml" >>> writeDocumentToString [withIndent yes, withRemoveWS no, withPreserveComment yes]

exp1 = do
  s <- runX expArrow
  putStr $ indentAnd160 4 $ head s
  withFile "/tmp/tmp2.xml" WriteMode $ \h -> do
    hPutStr h $ indentAnd160 4 $ head s


main = exp1
