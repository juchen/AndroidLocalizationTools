module Main where

import FromStringsXmls
import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as M
import System.Environment
import StringsFiles

setCell:: Int -> Int -> String -> Worksheet -> Worksheet
setCell i j s = cellValueAt (i, j) ?~ CellText (T.pack s)

setRow:: Int -> (TextKey, [TextContent]) -> Worksheet -> Worksheet
setRow n (k, ss) ws = foldr (f n) ws' (zip [2..] ss)
  where f:: Int -> (Int, TextContent) -> Worksheet -> Worksheet
        f i (j, s) = setCell i j s
        ws' = setCell n 1 k ws

setWorksheet:: [(TextKey, [TextContent])] -> Worksheet -> Worksheet
setWorksheet ls ws = foldr f ws (zip [1..] ls)
  where f:: (Int, (TextKey, [TextContent])) -> Worksheet -> Worksheet
        f (i, line) = setRow i line


pathsToXlsx:: [FilePath] -> IO Xlsx
pathsToXlsx paths = do
  bm <- bigMap paths
  let
    bigList=(("key", paths):(f3 paths bm))
    sheet = setWorksheet bigList def
    xlsx = def & atSheet (T.pack "Localization") ?~ sheet
  return xlsx
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


proc = do
  args <- getArgs
  if args == [] then
    putStr "\nUsage: to_xlsx <xlsx file>\n\n\tHas to be run in the src/main/res/ directory. Collect strings.xml files into an excel file.\n\n"
    else do
      ct <- getPOSIXTime
      xlsx <- (valuesDirs >>= pathsToXlsx)
      L.writeFile (head args) $ fromXlsx ct xlsx
      return ()

main = proc
