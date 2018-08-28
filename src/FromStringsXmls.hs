module FromStringsXmls where
--   (toCSV
--                       , BigMap
--                       , SmallMap
--                       , ContentMap
--                       , TextKey
--                       , TextContent
--                       , LangCode
--                       , XString
--                       , maybeStringFromXmlTree
--                       , stringFromBigMap) where

import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.Parser.XmlParsec as P
import Text.XML.HXT.DOM.FormatXmlTree
import Data.Tree.NTree.TypeDefs
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as Merge


parseFile:: String -> IO XmlTree
parseFile fname = fmap head $ runX $ (readDocument [withValidate no, withCurl []] fname)

isTagOf:: String -> XmlTree -> Bool
isTagOf s (NTree (XTag qn _) _)
  | (show qn) == "\"" ++ s ++ "\"" = True
  | otherwise = False
isTagOf _ _ = False

isAttrOf:: String -> XmlTree -> Bool
isAttrOf s (NTree (XAttr qn) _)
  | (show qn) == "\"" ++ s ++ "\"" = True
  | otherwise = False
isAttrOf _ _ = False

children:: IOLA String XmlTree
children = (IOLA (\valuesDir -> fmap (:[]) (parseFile (valuesDir ++ "/strings.xml"))) >>> getChildren >>> getChildren)

getXAttrName (NTree _ ((NTree (XText name) _):_)) = name



data XString = XString { keyXString:: String
                       , translatable:: Bool
                       , text:: String
                       } deriving Show

xStringFromXmlTree:: XmlTree -> XString
xStringFromXmlTree t@(NTree (XTag _ attrs) texts)
  | True == isTagOf "string" t = XString key translatable text
  | otherwise = error "Not a string entry"
    where key = getXAttrName $ head [attr | attr <- attrs, (isAttrOf "name" attr)]
          translatable = case [attr | attr <- attrs, (isAttrOf "translatable" attr)] of
            [] -> True
            tr:_ -> (getXAttrName tr == "true")
          text = case texts of
            (NTree (XText txt) _):_ -> txt
            _ -> ""
xStringFromXmlTree _ = error "Bad format"

maybeXStringFromXmlTree:: XmlTree -> Maybe XString
maybeXStringFromXmlTree t@(NTree (XTag _ _) _)
  | True == isTagOf "string" t = Just $ xStringFromXmlTree t
  | otherwise = Nothing
maybeStringFromXmlTree _ = Nothing

xmlTreeFromXString:: XString -> XmlTree
xmlTreeFromXString x = NTree (XTag (mkName "string")
                             ( -- attrs:: NTrees
                              (NTree (XAttr (mkName "name")) [NTree (XText k) []]):
                              if t_able == False then [NTree (XAttr (mkName "translatable")) [NTree (XText "false") []]]
                                                 else []
                             )) [NTree (XText t) []]
  where k = keyXString x
        t_able = translatable x
        t = text x

data XStringArray = XStringArray { keyXStringArray:: String
                                 , textArray:: [String]
                                 } deriving Show

xStringArrayFromXmlTree t@(NTree (XTag _ attrs) children)
  | True == isTagOf "string-array" t = XStringArray key textArray_
  | otherwise = error "Not a string array entry"
    where key = getXAttrName $ head [attr | attr <- attrs, (isAttrOf "name" attr)]
          textArray_ = map textFromXmlTree [x | x <- children, (isTagOf "item" x)]
          textFromXmlTree:: XmlTree -> String
          textFromXmlTree (NTree (XTag _ _) ((NTree (XText str) _):_)) = str
          textFromXmlTree _ = error "Not implemented yet"


inflate:: XStringArray -> [XString]
inflate (XStringArray k a) = foldr (\a acc -> (xStringFromItem a):acc) [] $ zip [0..] a
  where xStringFromItem (n, s) = XString { keyXString = k ++ "[" ++ (show n) ++ "]"
                                    , translatable = False
                                    , text = s}

inflateIO:: IOLA XStringArray XString
inflateIO = IOLA $ \x -> return (inflate x)

toMap:: [XString] -> M.Map String String
toMap l = M.fromList $ zip (map keyXString l) (map text l)


type BigMap = (M.Map TextKey (M.Map LangCode TextContent))
type SmallMap = M.Map TextKey TextContent
type ContentMap = M.Map LangCode TextContent
type LangCode = String
type TextKey = String
type TextContent = String


onlyInBigMap:: TextKey -> ContentMap -> Maybe ContentMap
onlyInBigMap _ m = Just m

onlyInSmallMap:: LangCode -> TextKey -> TextContent -> Maybe ContentMap
onlyInSmallMap lc _ v = Just $ M.singleton lc v

inBigAndSmallMap:: LangCode -> TextKey -> TextContent -> ContentMap -> Maybe ContentMap
inBigAndSmallMap lc _ v m = Just $ M.insert lc v m

mergeSmallIntoBig:: LangCode -> SmallMap -> BigMap -> BigMap
mergeSmallIntoBig lc = Merge.merge (Merge.mapMaybeMissing (onlyInSmallMap lc)) (Merge.mapMaybeMissing onlyInBigMap) (Merge.zipWithMaybeMatched (inBigAndSmallMap lc))

mapFromAValuesDir:: FilePath -> IO SmallMap
mapFromAValuesDir dname = do
  p <- runIOLA  (children >>> (isA $ isTagOf "string") >>> (arr xStringFromXmlTree) >>> (isA translatable)) $ dname
  return $ toMap p


parseValueDirs:: [FilePath] -> IO [(LangCode, SmallMap)]
parseValueDirs l = do
  mapM f l
    where f dname = fmap (\x -> (dname, x)) (mapFromAValuesDir dname)

mergeResult:: [(LangCode, SmallMap)] -> BigMap
mergeResult = foldr (\(lc, smallMap) bm ->mergeSmallIntoBig lc smallMap bm) M.empty

bigMap:: [FilePath] -> IO BigMap
bigMap paths = fmap mergeResult (parseValueDirs paths)

stringFromBigMap:: LangCode -> BigMap -> TextKey -> Maybe (Maybe String)
stringFromBigMap lc bm k = do
  cm <- M.lookup k bm
  return $ do
    M.lookup lc cm

toCSV:: [FilePath] -> IO ()
toCSV paths = do
  bm <- bigMap paths
  _ <- mapM (putStrLn.f4) (("key", paths):(f3 paths bm))
  return ()
  where
    -- A tab separated CSV is used because the can be comma in some strings.
    f4:: (TextKey, [TextContent]) -> String
    f4 (k, l) = foldl (\b a -> b ++ "\t" ++ a ++ "") k l
    f3:: [LangCode] -> BigMap -> [(TextKey, [TextContent])]
    f3 l bm = map (\(x1, x2) -> (x1, (f l x2))) (f2 bm)
    f2:: BigMap -> [(TextKey, ContentMap)]
    f2 bm = M.toList bm
    f:: [LangCode] -> ContentMap -> [TextContent]
    f l cm = map (\k -> g (M.lookup k cm)) l
      where g (Just t) = t
            g Nothing = ""



