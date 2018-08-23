{-# LANGUAGE OverloadedStrings #-}
module Main where
import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Map as M
import FromStringsXmls
import FromXlsx

expFile = "/tmp/localization.xlsx"

exp0 = do
  bs <- L.readFile "/tmp/localization.xlsx"
  let (Just (CellText value)) = toXlsx bs ^? ixSheet "tmp" .
              ixCell (3,5) . cellValue . _Just
  putStrLn $ "Cell B3 contains " ++ T.unpack value


exp1 fname = do
  bs <- L.readFile fname
  putStrLn $ foldr f "" $ map getTextString $ ((getRow 1).getCellMap.toXlsx) bs
    where f s acc = s ++ "," ++ acc

my_to_csv fname = do
  bs <- L.readFile fname
  let cm = getCellMap.toXlsx $ bs
  let bm = fromCellMap cm
  let _:paths = map getTextString $ getRow 1 cm
  _ <- mapM (putStrLn.f4) (("key", paths):(f3 paths bm))
  return ()
    where
      -- A tab separated CSV is used because there can be commas in some strings.
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
main = my_to_csv expFile
