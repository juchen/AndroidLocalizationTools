module FromXlsx where

import Codec.Xlsx
import qualified Data.Map as M
import qualified Data.Text as T
import FromStringsXmls
import qualified Data.ByteString.Lazy as L

getCellMap:: Xlsx -> CellMap
getCellMap = _wsCells.snd.head._xlSheets

getRow:: Int -> CellMap -> [Cell]
getRow n cm = f [1..]
  where f (i:is) = case M.lookup (n, i) cm of
          Just a -> a:(f is)
          Nothing -> []
        f [] = []

lookupRow:: Int -> CellMap -> Maybe (Int -> Maybe String)
lookupRow n cm = M.lookup (n, 1) cm >> Just f
    where f i = M.lookup (n, i) cm >>= maybeGetTextString
--   Just cell -> Just f
--     where f i = case M.lookup (n, i) cm of
--             Just cell -> getTextString cell
--             Nothing -> ""
--   Nothing -> Nothing

maybeGetTextString:: Cell -> Maybe String
maybeGetTextString (Cell _ (Just (CellText t)) _ _)
  | T.unpack t == "" = Nothing
  | otherwise = Just $ T.unpack t
maybeGetTextString _ = Nothing

getTextString:: Cell -> String
getTextString (Cell _ (Just (CellText t)) _ _) = T.unpack t
getTextString _ = error "Bad cell. Not a text cell"

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
        f [] = []

bigMapFromFile:: FilePath -> IO BigMap
bigMapFromFile fn = do
  bs <- L.readFile fn
  return $ (fromCellMap.getCellMap.toXlsx) bs


