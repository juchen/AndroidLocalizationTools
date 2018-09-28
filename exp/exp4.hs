module Main where

import System.Environment
import System.IO
import FromXlsx
import StringsFiles
import MergeXlsxToXml
import Text.XML.HXT.Core
import Text.Parsec

expArrow:: IOStateArrow s b String
expArrow = readDocument [] "/tmp/tmp.xml" >>> writeDocumentToString [withIndent yes, withRemoveWS no, withPreserveComment yes]

exp1 = do
  s <- runX expArrow
  putStr $ indentAnd160 4 $ head s
  withFile "/tmp/tmp2.xml" WriteMode $ \h -> do
    hPutStr h $ indentAnd160 4 $ head s

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

indentAnd160:: Int -> String -> String
indentAnd160 i xml = either (\_ -> "Error") (prettyShow i) $ runParser xmlText () "" xml

main = exp1
