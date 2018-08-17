import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.Parser.XmlParsec as P
import Text.XML.HXT.DOM.FormatXmlTree
import StringsFiles
import Data.Tree.NTree.TypeDefs


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

expFile = fmap (!!0) stringsFiles

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

children:: IOLA a XmlTree
children = (IOLA (const $ fmap (:[]) (parseFile =<< expFile)) >>> getChildren >>> getChildren)

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
          text = ""
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

exp4 = do
  str <- runIOLA  (children >>> (isA $ isTagOf "string")) $ undefined
  array <- runIOLA (children >>> (isA $ isTagOf "string-array")) $ undefined
  _ <- mapM (putStr.formatXmlTree) $ str
  _ <- mapM (putStr.formatXmlTree) $ array
  return ()

exp7 = do
  p <- runIOLA  (children >>> (isA $ \t -> (isTagOf "string" t) || (isTagOf "string-array" t))) $ undefined
  _ <- mapM (putStr.formatXmlTree) $ p
  return ()

exp8 = do
  p <- runIOLA  (children >>> (isA $ isTagOf "string")) $ undefined
--  print $ map xStringFromXmlTree p
  _ <- mapM (putStr.formatXmlTree) $ p
  return ()

exp9 = do
  p <- runIOLA  (children >>> (isA $ isTagOf "string-array")) $ undefined
  print $ map xStringArrayFromXmlTree p
--  _ <- mapM (putStr.formatXmlTree) $ p
  return ()

main = exp9

