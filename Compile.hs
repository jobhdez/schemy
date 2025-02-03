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


compile :: String -> IO ()
compile exp =
  let ast = toAst (lexer exp)
      anf = toAnf (desugar ast)
      closr = closure anf 0
  in print closr

main = do
  s <- readFile "testx.scm"
  compile s

