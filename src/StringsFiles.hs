module StringsFiles(stringsFiles
                   , valuesDirs) where

import System.Directory
-- import Control.Arrow.ListArrow
import Control.Arrow.IOListArrow
import Control.Arrow
import Data.List

filterFrom:: (a -> IO Bool) -> IOLA a a
filterFrom f = IOLA $ \a -> do
  c <- f a
  return $ case c of
    True -> [a]
    False -> []


filterTypes:: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
filterTypes f l = runIOLA a undefined where
  a = (IOLA (const (return l))) >>> (filterFrom f)

isAValuesDir:: FilePath -> IO Bool
isAValuesDir path = do
  b1 <- doesDirectoryExist path
  b2 <- doesFileExist (path ++ "/strings.xml")
  return (b1 && b2 && beginWithValues)
    where beginWithValues = "values" == take 6 path

listDirectoriesOf:: (FilePath -> IO Bool) -> IO [FilePath]
listDirectoriesOf f = do
  l <- (getCurrentDirectory >>= getDirectoryContents)
  filterTypes f l

valuesDirs:: IO [FilePath]
valuesDirs = fmap sort $ listDirectoriesOf isAValuesDir

stringsFiles:: IO [FilePath]
stringsFiles = do
  vs <- valuesDirs
  let l = map (++ "/strings.xml") vs
  filterTypes doesFileExist l


  -- main:: IO ()
-- main = stringsFiles >>= print

