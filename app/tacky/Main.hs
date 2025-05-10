module Main (main) where

import Lexer.Lexer (lexerIO)
import Parser.Parser (evalParseIO)
import SemanticAnalysis.LoopLabeling
import SemanticAnalysis.VariableResolution
import System.Environment (getArgs)
import TACKY.TACKY (genLProgram)
import Utils.Pretty (prettyPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      ast <- lexerIO content >>= evalParseIO >>= resolveProgramIO >>= labelProgramIO
      let ast' = genLProgram ast
      putStrLn $ prettyPrint ast'
    _ -> putStrLn "Usage: hsc-tacky <file-path>"