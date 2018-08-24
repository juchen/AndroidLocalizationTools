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
                  Just u -> y { text = unJust u }
                  Nothing -> y

-- A fail trial.
replaceText':: BigMap -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
replaceText' bm lc = changeUserState removeUsed >>> isA (not.shouldRemove)
                     >>> (getName &&& returnA)
                     >>> ((\f -> (arr $ \(k, x) -> head $ (runLA $ f k) x)) $ \k ->
                             processBottomUp ((isText >>> (case stringFromBigMap lc bm k of
                                          Just u -> txt (unJust u)
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

unJust (Just x) = x
unJust Nothing = error "Bad situation"

addRestStrings:: LangCode -> IOStateArrow s (BigMap, XmlTree) XmlTree
addRestStrings lc = IOSLA $ (\s (bm, x) -> (runIOSLA (accBigMap bm lc)) s x)
  where accBigMap:: BigMap -> LangCode -> IOStateArrow s XmlTree XmlTree
        accBigMap bm lc = foldr cc returnA (fromBigMap bm lc)
        fromContentMap:: LangCode -> (TextKey, ContentMap) -> Maybe (TextKey, TextContent)
        fromContentMap lc (k, m) = do
          v <- M.lookup k m
          return (k, v)
        fromBigMap::BigMap -> LangCode ->[(TextKey, TextContent)]
        fromBigMap bm lc = map unJust [c | c <- map (fromContentMap lc) (M.toList bm), c /= Nothing]
        cc:: (TextKey, TextContent) -> IOStateArrow s XmlTree XmlTree -> IOStateArrow s XmlTree XmlTree
        cc (k, c) a = a += (mkelem "string" [sattr "name" k] [txt c])

stringsXmlArrow:: BigMap -> FilePath -> FilePath -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
stringsXmlArrow bm inFile outFile lc = readDocument readConfig inFile
                            >>> processWithLangCode bm lc
                            >>> processBottomUp ((isElem >>> hasName "resources"
                                                  >>> (getUserState &&& returnA)
                                                  >>> addRestStrings lc
                                                  ) `orElse` returnA
                                                )
                            >>> writeDocument writeConfig outFile

stringsXmlConversion:: BigMap -> FilePath -> FilePath -> LangCode -> IO [XmlTree]
stringsXmlConversion  bm inFile outFile lc =
  runXIOState (initialState bm) (stringsXmlArrow bm inFile outFile lc)

renameFileArrow:: IOLA (FilePath, FilePath) (FilePath, FilePath)
renameFileArrow = IOLA $ \(old, new) -> do
  renameFile old new
  return [(old, new)]

exp2 = do
  xmlFileName:_ <- getArgs
  bm <- bigMapFromFile xmlFileName
  restBm <- runXIOState (initialState bm) $ (stringsXmlArrow bm "/tmp/in.xml" "/tmp/out.xml" "values-ja-rJP")
                                            >>> getUserState
  print $ length.(M.toList).head $ restBm
  return ()

exp3 = do
  xmlFileName:_ <- getArgs
  bm <- bigMapFromFile xmlFileName
  (\f -> runIOLA f undefined) $ (IOLA $ const valuesDirs)
    >>> proc d -> do
          old <- arr (++ "/strings.xml") -< d
          new <- arr (++ "/strings.orig.xml") -< d
          _ <- renameFileArrow -< (old, new)
          (IOLA $ \(lc, inf, outf) -> stringsXmlConversion bm inf outf lc) -< (d, new, old)
  return ()

exp4 = do
  xmlFileName:_ <- getArgs
  bm <- bigMapFromFile xmlFileName
  stringsXmlConversion bm "/tmp/in.xml" "/tmp/out.xml" "values-zh-rCN"

main = exp3

