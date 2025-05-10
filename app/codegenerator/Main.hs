module Main (main) where

import AssemblyGen.AssemblyGen (convertProgramWithFixedInstructions)
import Lexer.Lexer (lexerIO)
import Parser.Parser (evalParseIO)
import SemanticAnalysis.VariableResolution
import System.Environment (getArgs)
import TACKY.TACKY (genLProgram)
import Utils.Pretty (prettyPrint)
import SemanticAnalysis.LoopLabeling

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      ast <- lexerIO content >>= evalParseIO >>= resolveProgramIO >>= labelProgramIO
      let p = convertProgramWithFixedInstructions (genLProgram ast) 
      putStrLn $ prettyPrint p
    _ -> putStrLn "Usage: hsc-codegenerator <file-path>"
