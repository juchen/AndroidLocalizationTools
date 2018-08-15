import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.DOM.FormatXmlTree

main = do
  r:rs <- fmap xread getContents
  putStr $ formatXmlTree $ r
  -- print rs

