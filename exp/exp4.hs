module Main where

import System.Environment
import FromXlsx
import StringsFiles
import MergeXlsxToXml
import Text.XML.HXT.Core
import Text.Parsec

expArrow:: IOStateArrow s b String
expArrow = readDocument [] "/tmp/tmp.xml" >>> writeDocumentToString [withIndent yes, withRemoveWS no, withPreserveComment yes]

exp1 = do
  s <- runX expArrow
  putStr $ either (\_ -> "Error") (prettyShow 4) $ runParser xmlText () "" $ head s

leadingSpace:: Parsec String u Int
leadingSpace = fmap length $ many (char ' ')

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


main = exp1
