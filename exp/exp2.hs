{-# LANGUAGE Arrows #-}
module Main where

import Text.XML.HXT.Core
import FromStringsXmls
import FromXlsx
import qualified Data.Map.Strict as M
import System.Environment
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Data.Tree.NTree.TypeDefs

parseFile' fname = runX $ (readDocument readConfig fname)
                          >>> (processBottomUp $ (isElem
                                >>> hasName "string"
                                >>> hasAttrValue "translatable" ((==) "false")
                                >>> mkelem "hello" [] [])
                               `orElse` (returnA))
                          >>> (writeDocument writeConfig "/tmp/out.xml")

copyXml fname = runX $ (readDocument [withValidate no] fname)
                        >>> (writeDocument [withIndent yes] "/tmp/out.xml")


readConfig:: [SysConfig]
readConfig = [withValidate no
              , withRemoveWS no
              , withPreserveComment yes
              , withIgnoreNoneXmlContents no]

writeConfig:: [SysConfig]
writeConfig = [withIndent yes
              , withRemoveWS no
              , withPreserveComment yes]


replaceText:: BigMap -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
replaceText bm lc = changeUserState removeUsed >>> isA (not.shouldRemove) >>> arr g
  where
    removeUsed:: XmlTree -> BigMap -> BigMap
    removeUsed x bm = M.delete (keyXString $ xStringFromXmlTree x) bm
    shouldRemove:: XmlTree -> Bool
    shouldRemove x = case stringFromBigMap lc bm (keyXString $ xStringFromXmlTree x) of
      Just Nothing -> True
      _ -> False
    g:: XmlTree -> XmlTree
    g x = xmlTreeFromXString $ y'
      where y = xStringFromXmlTree x
            y' = case (stringFromBigMap lc bm (keyXString y)) of
                  Just (Just u) -> y { text = u }
                  Just Nothing -> error "Should not be used under this situation"
                  Nothing -> y

-- A fail trial.
replaceText':: BigMap -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
replaceText' bm lc = changeUserState removeUsed >>> isA (not.shouldRemove)
                     >>> (getName &&& returnA)
                     >>> ((\f -> (arr $ \(k, x) -> head $ (runLA $ f k) x)) $ \k ->
                             processBottomUp ((isText >>> (case stringFromBigMap lc bm k of
                                          Just (Just u) -> txt u
                                          Just Nothing -> error "Should not be used under this situation"
                                          Nothing -> returnA)) `orElse` returnA))
  where
    removeUsed:: XmlTree -> BigMap -> BigMap
    removeUsed x bm = M.delete (keyXString $ xStringFromXmlTree x) bm
    shouldRemove:: XmlTree -> Bool
    shouldRemove x = case stringFromBigMap lc bm (keyXString $ xStringFromXmlTree x) of
      Just Nothing -> True
      _ -> False

processWithLangCode:: BigMap -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
processWithLangCode bm lc = processBottomUp $
  (isElem >>> hasName "string" >>> (replaceText bm lc)) `orElse` returnA
-- processWithLangCode _ _ = processBottomUp $
--    (isA isStringElem) `orElse` returnA
--      where isStringElem (NTree (XTag s _) _) = ((show s) == "string")
--            isStringElem _ = False



exp1 = do
  xmlFileName:_ <- getArgs
  bm <- bigMapFromFile xmlFileName
  restBm <- runXIOState (initialState bm) $ readDocument readConfig "/tmp/in.xml"
                                              >>> processWithLangCode bm "values-ja-rJP"
                                              >>> writeDocument writeConfig "/tmp/out.xml"
                                              >>> getUserState
  print $ length.(M.toList).head $ restBm
  return ()


main = exp1

