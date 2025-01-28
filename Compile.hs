module Compile where
import System.IO
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

-- Compile function that desugars, converts to ANF, performs closure conversion, and then selects instructions.
compile :: String -> [Instruction]
compile exp =
  let ast = toAst (lexer exp)
      anf = toAnf (desugar ast)
      (closure, _) = closureConversion anf  -- Ignore the environment if it's not needed
  in toselect' closure 0

-- Main function for reading a file and compiling it.
main :: IO ()
main = do
    putStrLn "Enter the file path:"
    filePath <- getLine  -- Get the file path from the user
    fileContent <- readFile filePath  -- Read the file content
    let instructions = compile fileContent  -- Compile the content
    print instructions  -- Print the instructions
