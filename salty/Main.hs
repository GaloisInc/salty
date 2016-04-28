
import Scope.Check
import Syntax.Lexer
import Syntax.Parser

import qualified Data.Text.Lazy.IO as L

main :: IO ()
main  =
  do bytes <- L.readFile "test.salt"
     mapM_ print (lexWithLayout "test.salt" bytes)
     let Right cont = parseController "test.salt" bytes

     let scCont = scopeCheck cont

     print scCont
