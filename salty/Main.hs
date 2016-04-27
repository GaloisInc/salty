
import Syntax.Lexer
import Syntax.Parser

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

main =
  do bytes <- L.readFile "test.salt"
     mapM_ print (lexWithLayout "test.salt" bytes)
     print (parseController "test.salt" bytes)
