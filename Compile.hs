module Compile where
import System.IO
import Parser (
  Exp(DefineProc),
  lexer,
  toAst
  )
import Desugar (desugar)
import ToAnf (toAnf)
import ClosureConversion (closure)
import ToSelect (toselect)

compile :: String -> IO ()
compile exp =
  let ast = toAst (lexer exp)
      anf = toAnf ast
      closr = closure anf 0
      selects = toselect closr 0
  in print closr

main = do
  s <- readFile "test2.scm"
  compile s

