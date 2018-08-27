{-# LANGUAGE Arrows #-}
module Main where

import Text.XML.HXT.Core
import FromStringsXmls
import FromXlsx
import qualified Data.Map.Strict as M
import System.Environment
import System.Directory
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Data.Tree.NTree.TypeDefs
import StringsFiles
import MergeXlsxToXml

parseFile' fname = runX $ (readDocument readConfig fname)
                          >>> (processBottomUp $ (isElem
                                >>> hasName "string"
                                >>> hasAttrValue "translatable" ((==) "false")
                                >>> mkelem "hello" [] [])
                               `orElse` (returnA))
                          >>> (writeDocument writeConfig "/tmp/out.xml")

copyXml fname = runX $ (readDocument [withValidate no] fname)
                        >>> (writeDocument [withIndent yes] "/tmp/out.xml")

exp1 = do
  xmlFileName:_ <- getArgs
  bm <- bigMapFromFile xmlFileName
  restBm <- runXIOState (initialState bm) $ readDocument readConfig "/tmp/in.xml"
                                              >>> processTextReplacement bm "values-ja-rJP"
                                              >>> writeDocument writeConfig "/tmp/out.xml"
                                              >>> getUserState
  print $ length.(M.toList).head $ restBm
  return ()

exp2 = do
  xlsxFn:_ <- getArgs
  bm <- bigMapFromFile xlsxFn
  restBm <- runXIOState (initialState bm) $ (stringsXmlArrow bm "/tmp/in.xml" "/tmp/out.xml" "values-zh-rCN")
                                            >>> getUserState
  print $ head restBm
  print $ length.(M.toList).head $ restBm
  return ()

exp3 = do
  xlsxFn:_ <- getArgs
  bm <- bigMapFromFile xlsxFn
  runIOLA ((IOLA (const valuesDirs)) >>> (mergeXlsxToXml bm)) undefined
  return ()

exp4 = do
  xlsxFn:_ <- getArgs
  bm <- bigMapFromFile xlsxFn
  stringsXmlConversion bm "/tmp/in.xml" "/tmp/out.xml" "values-zh-rCN"

main = exp3

