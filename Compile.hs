module Compile where

import Parser (
  Exp,
  lexer,
  toAst
  )

import Desugar (desugar)
import ToAnf (toAnf)
import ClosureConversion (closureConversion)
import ToSelect (
  Instruction,
  toselect'
  )

compile :: String -> [Instruction]
compile exp =
  let ast = toAst (lexer exp)
      anf = toAnf (desugar ast)
      (closure, env) = closureConversion anf in
    toselect' closure 0
