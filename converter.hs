import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.Parser.XmlParsec as P
import Text.XML.HXT.DOM.FormatXmlTree
import StringsFiles
import Data.Tree.NTree.TypeDefs
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge


exp1 = do
  r:rs <- fmap P.xread getContents
  putStr $ formatXmlTree $ r
  -- print rs

exp2 = do
  src_s <- stringsFiles
  [dst] <- getArgs
  runX $
    readDocument [withValidate no
                 ,withCurl []
                 ] (src_s!!3)
    >>> getChildren >>>
    writeDocument [withIndent yes
                  ,withOutputEncoding utf8
                  ] dst
  return ()

parseFile:: String -> IO XmlTree
parseFile fname = fmap head $ runX $ (readDocument [withValidate no, withCurl []] fname)

-- expFile = fmap (!!3) stringsFiles
-- expFile = return "/tmp/strings.xml"
expValuesPath = fmap (!!3) valuesDirs
expFile = fmap (++ "strings.xml") expValuesPath

exp3 = do
  (fmap length $ parseFile =<< expFile) >>= print


exp5 = do
  NTree _ ((NTree _ rs):_) <- parseFile =<< expFile
  print (length rs)

exp6 = do
  fname <- expFile
  r <- runX $ ((readDocument [withValidate no, withCurl []] fname) >>> getChildren)
  print (length r)

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

toMap:: [XString] -> Map.Map String String
toMap l = Map.fromList $ zip (map keyXString l) (map text l)

exp4 = do
  valuesDir <- expValuesPath
  str <- runIOLA  (children
                   >>> (isA $ isTagOf "string")
                   >>> (arr xStringFromXmlTree)
                  ) $ valuesDir
  array <- runIOLA (children
                    >>> (isA $ isTagOf "string-array")
                    >>> (arr xStringArrayFromXmlTree)
                    >>> inflateIO
                   ) $ valuesDir
--  _ <- mapM (putStr.formatXmlTree) $ str
--  _ <- mapM (putStr.formatXmlTree) $ array
  return $ str ++ array

exp7 = do
  valuesDir <- expValuesPath
  p <- runIOLA  (children >>> (isA $ \t -> (isTagOf "string" t) || (isTagOf "string-array" t))) $ valuesDir
  _ <- mapM (putStr.formatXmlTree) $ p
  return ()

exp8 = expValuesPath >>= runIOLA  (children >>> (isA $ isTagOf "string"))
--   print $ map xStringFromXmlTree p
--  _ <- mapM (putStr.formatXmlTree) $ p
--  return ()

exp9 = expValuesPath >>= runIOLA  (children >>> (isA $ isTagOf "string-array") >>> (arr xStringArrayFromXmlTree) >>> inflateIO)
--  _ <- mapM (putStr.formatXmlTree) $ p

exp10 = expValuesPath >>=
  runIOLA  (children >>> (isA $ isTagOf "string") >>> (arr xStringFromXmlTree) >>> (isA translatable))


type BigMap = (Map.Map TextKey (Map.Map LangCode TextContent))
type SmallMap = Map.Map TextKey TextContent
type ContentMap = Map.Map LangCode TextContent
type LangCode = String
type TextKey = String
type TextContent = String


onlyInBigMap:: TextKey -> ContentMap -> Maybe ContentMap
onlyInBigMap _ m = Just m

onlyInSmallMap:: LangCode -> TextKey -> TextContent -> Maybe ContentMap
onlyInSmallMap lc _ v = Just $ Map.singleton lc v

inBigAndSmallMap:: LangCode -> TextKey -> TextContent -> ContentMap -> Maybe ContentMap
inBigAndSmallMap lc _ v m = Just $ Map.insert lc v m

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
mergeResult = foldr (\(lc, smallMap) bigMap ->mergeSmallIntoBig lc smallMap bigMap) Map.empty

bigMap:: [FilePath] -> IO BigMap
bigMap paths = fmap mergeResult (parseValueDirs paths)

toCSV paths = do
  bm <- bigMap paths
  l <- valuesDirs
  mapM (putStrLn.f4) (("key", l):(f3 l bm))
  where
    -- A tab separated CSV is used because the can be comma in some strings.
    f4:: (TextKey, [TextContent]) -> String
    f4 (k, l) = foldl (\b a -> b ++ "\t\"" ++ a ++ "\"") k l
    f3:: [LangCode] -> BigMap -> [(TextKey, [TextContent])]
    f3 l bm = map (\(x1, x2) -> (x1, (f l x2))) (f2 bm)
    f2:: BigMap -> [(TextKey, ContentMap)]
    f2 bm = Map.toList bm
    f:: [LangCode] -> ContentMap -> [TextContent]
    f l cm = map (\k -> g (Map.lookup k cm)) l
      where g (Just t) = t
            g Nothing = ""

-- main = expValuesPath >>= mapFromAValuesDir >>= print
main = valuesDirs >>= toCSV
-- main = valuesDirs >>= print


