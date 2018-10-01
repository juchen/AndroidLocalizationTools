{-# LANGUAGE Arrows #-}
module MergeXlsxToXml where

import System.IO
import Text.XML.HXT.Core
import FromStringsXmls
import qualified Data.Map.Strict as M
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import System.Directory
import Text.Parsec

unJust:: Maybe a -> a
unJust (Just x) = x
unJust Nothing = error "Bad situation"

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
replaceText bm lc = changeUserState removeUsed >>> (ifA (hasAttrValue "translatable" (== "false"))
                                                        returnA
                                                        (isA (not.shouldRemove) >>> arr g))
  where
    removeUsed:: XmlTree -> BigMap -> BigMap
    removeUsed x bm = M.delete (keyXString $ xStringFromXmlTree x) bm
    shouldRemove:: XmlTree -> Bool
    shouldRemove x = case stringFromBigMap lc bm (keyXString $ xStringFromXmlTree x) of
      Just Nothing -> lc /= "values"
      _ -> False
    g:: XmlTree -> XmlTree
    g x = xmlTreeFromXString $ y'
      where y = xStringFromXmlTree x
            y' = case (stringFromBigMap lc bm (keyXString y)) of
                  Just (Just u) -> y { text = u }
                  Just Nothing -> y { text = "" }
                  Nothing -> y

processTextReplacement:: BigMap -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
processTextReplacement bm lc = processBottomUp $
  ifA (isElem >>> hasName "string")
      (replaceText bm lc)
      returnA

addRestStrings:: LangCode -> IOStateArrow s (BigMap, XmlTree) XmlTree
addRestStrings lc = IOSLA $ (\s (bm, x) -> (runIOSLA (accBigMap bm lc)) s x)
  where accBigMap:: BigMap -> LangCode -> IOStateArrow s XmlTree XmlTree
        accBigMap bm lc = foldr cc returnA (fromBigMap bm lc)
        fromContentMap:: LangCode -> (TextKey, ContentMap) -> Maybe (TextKey, TextContent)
        fromContentMap lc (k, m) = do
          v <- M.lookup lc m
          return (k, v)
        fromBigMap::BigMap -> LangCode ->[(TextKey, TextContent)]
        fromBigMap bm lc = map unJust [c | c <- map (fromContentMap lc) (M.toList bm), c /= Nothing]
        cc:: (TextKey, TextContent) -> IOStateArrow s XmlTree XmlTree -> IOStateArrow s XmlTree XmlTree
        cc (k, c) a = a += (mkelem "string" [sattr "name" k] [txt c])

processAddingText:: LangCode -> IOStateArrow BigMap XmlTree XmlTree
processAddingText lc = processBottomUp ((isElem >>> hasName "resources"
                                             >>> (getUserState &&& returnA)
                                             >>> addRestStrings lc) `orElse` returnA)

readAndProcessXmlArrow:: BigMap -> FilePath -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
readAndProcessXmlArrow bm inFile lc = readDocument readConfig inFile
                                >>> processTextReplacement bm lc
                                >>> processAddingText lc

-- Write to a file directly.
stringsXmlArrow:: BigMap -> FilePath -> FilePath -> LangCode -> IOStateArrow BigMap XmlTree XmlTree
stringsXmlArrow bm inFile outFile lc = readAndProcessXmlArrow bm inFile lc
                                       >>> writeDocument writeConfig outFile

-- Convert to a string for further processing.
stringsXmlArrowString:: BigMap -> FilePath -> LangCode -> IOStateArrow BigMap XmlTree String
stringsXmlArrowString bm inFile lc = readAndProcessXmlArrow bm inFile lc
                                     >>> writeDocumentToString writeConfig


-- Convert into file directly without a post process.
stringsXmlConversion:: BigMap -> FilePath -> FilePath -> LangCode -> IO [XmlTree]
stringsXmlConversion  bm inFile outFile lc =
  runXIOState (initialState bm) (stringsXmlArrow bm inFile outFile lc)

-- Convert to string and do a post process for indentation and special char substitution.
stringsXmlConversionPP:: Int -> BigMap -> FilePath -> FilePath -> LangCode -> IO [XmlTree]
stringsXmlConversionPP tabwidth bm inFile outFile lc = do
  s:_ <- runXIOState (initialState bm) (stringsXmlArrowString bm inFile lc)
  withFile outFile WriteMode $ \h -> do
    hPutStr h $ indentAnd160 tabwidth s
  return []

renameFileArrow:: IOLA (FilePath, FilePath) (FilePath, FilePath)
renameFileArrow = arrIO $ \(old, new) -> do
  renameFile old new
  return (old, new)

mergeXlsxToXml:: BigMap -> IOLA FilePath XmlTree
mergeXlsxToXml bm = proc d -> do
          old <- arr (++ "/strings.xml") -< d
          new <- arr (++ "/strings.orig.xml") -< d
          _ <- renameFileArrow -< (old, new)
          x <- (IOLA $ \(lc, inf, outf) -> stringsXmlConversionPP 4 bm inf outf lc) -< (d, new, old)
          _ <- (arrIO removeFile) -< new
          returnA -< x

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


--- For post process ---------------------------------------------------------------

nbsp = '\160'

transNbsp:: Parsec String u String
transNbsp = do
  _ <- char nbsp
  return "&#160;"

trailingText:: Parsec String u String
trailingText = do
  fmap concat $ many (try transNbsp <|> (fmap (:[]) $ noneOf [nbsp, '\n']))

xmlText:: Parsec String u [(Int, String)]
xmlText = manyTill aline eof
  where aline = do
                  i <- leadingSpace
                  s <- trailingText
                  _ <- try (char '\n')
                  return (i `div` 2, s)

prettyShow:: Int -> [(Int, String)] -> String
prettyShow tabwidth ls = concat $ map prettyLine ls
  where
    prettyLine:: (Int, String) -> String
    prettyLine (i, s) = (concat $ take i $ repeat (take tabwidth $ repeat ' ')) ++ s ++ "\n"

indentAnd160:: Int -> String -> String
indentAnd160 i xml = either (\_ -> "Error") (prettyShow i) $ runParser xmlText () "" xml

leadingSpace:: Parsec String u Int
leadingSpace = fmap length $ many (char ' ')
