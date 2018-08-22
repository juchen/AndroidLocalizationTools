-- import Data.Xls
-- import Conduit
-- import Data.List
-- 
-- -- exp :: Conduit i [String] IO ()
-- -- exp = decodeXls "/tmp/localization.xlsx"
-- 
-- 
-- xlsToCSV :: String -> IO ()
-- xlsToCSV file =
--       runResourceT
--     $ decodeXls file
--     $$ mapM_ (liftIO . putStrLn . intercalate ",")

-- main = (xlsToCSV "/tmp/localization.xlsx") >> return ()

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M
import FromStringsXmls

expFile = "/tmp/localization.xlsx"

exp0 = do
  bs <- L.readFile "/tmp/localization.xlsx"
  let (Just (CellText value)) = toXlsx bs ^? ixSheet "tmp" .
              ixCell (3,5) . cellValue . _Just
  putStrLn $ "Cell B3 contains " ++ T.unpack value

getCellMap:: Xlsx -> CellMap
getCellMap = _wsCells.snd.head._xlSheets

getRow:: Int -> CellMap -> [Cell]
getRow n cm = f [1..]
  where f (i:is) = case M.lookup (n, i) cm of
          Just a -> a:(f is)
          Nothing -> []

lookupRow:: Int -> CellMap -> Maybe (Int -> Maybe String)
lookupRow n cm = M.lookup (n, 1) cm >> Just f
    where f i = M.lookup (n, i) cm >>= maybeGetTextString
--   Just cell -> Just f
--     where f i = case M.lookup (n, i) cm of
--             Just cell -> getTextString cell
--             Nothing -> ""
--   Nothing -> Nothing

parseRow:: Int -> CellMap -> Maybe (TextKey, ContentMap)
parseRow n cm = do
  k <- (M.lookup (n, 1) cm >>= maybeGetTextString)
  f <- lookupRow n cm
  let h@(_:langCodes) = map getTextString $ getRow 1 cm -- header
  let maybeTexts = map f [2..(length h)]
  let l = zip langCodes maybeTexts
  let fl = foldr g [] l
  return (k, M.fromList fl)
    where
      g (lc, Just s) acc = (lc, s):acc
      g _ acc = acc

fromCellMap:: CellMap -> BigMap
fromCellMap cm = (M.fromList).f $ [2..]
  where f (n:ns) = case parseRow n cm of
          Just e -> e:(f ns)
          Nothing -> []


maybeGetTextString:: Cell -> Maybe String
maybeGetTextString (Cell _ (Just (CellText t)) _ _) = Just $ T.unpack t
maybeGetTextString _ = Nothing

getTextString:: Cell -> String
getTextString (Cell _ (Just (CellText t)) _ _) = T.unpack t
getTextString _ = error "Bad cell. Not a text cell"

exp1 fname = do
  bs <- L.readFile fname
  putStrLn $ foldr f "" $ map getTextString $ ((getRow 1).getCellMap.toXlsx) bs
    where f s acc = s ++ "," ++ acc

exp2 fname = do
  bs <- L.readFile fname
  let cm = getCellMap.toXlsx $ bs
  let bm = fromCellMap cm
  let _:paths = map getTextString $ getRow 1 cm
  _ <- mapM (putStrLn.f4) (("key", paths):(f3 paths bm))
  return ()
    where
      -- A tab separated CSV is used because the can be comma in some strings.
      f4:: (TextKey, [TextContent]) -> String
      f4 (k, l) = foldl (\b a -> b ++ "\t\"" ++ a ++ "\"") k l
      f3:: [LangCode] -> BigMap -> [(TextKey, [TextContent])]
      f3 l bm = map (\(x1, x2) -> (x1, (f l x2))) (f2 bm)
      f2:: BigMap -> [(TextKey, ContentMap)]
      f2 bm = M.toList bm
      f:: [LangCode] -> ContentMap -> [TextContent]
      f l cm = map (\k -> g (M.lookup k cm)) l
        where g (Just t) = t
              g Nothing = ""

main :: IO ()
main = exp2 expFile
