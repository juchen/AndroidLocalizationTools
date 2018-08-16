import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.Parser.XmlParsec as P
import Text.XML.HXT.DOM.FormatXmlTree


exp1 = do
  r:rs <- fmap P.xread getContents
  putStr $ formatXmlTree $ r
  -- print rs

exp2 = do
  [src, dst] <- getArgs
  runX $
    readDocument [withValidate no
                 ,withCurl []
                 ] src
    >>>
    writeDocument [withIndent yes
                  ,withOutputEncoding utf8
                  ] dst
  return ()

main = exp2
