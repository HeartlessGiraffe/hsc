module Main (main) where

import AssemblyGen.AssemblyGen (convertProgramWithFixedInstructions)
import Lexer.Lexer (lexerIO)
import Parser.Parser (evalParseIO)
import TACKY.TACKY
import System.Environment (getArgs)
import Emission.Emission
import System.FilePath (replaceExtension)
import SemanticAnalysis.LoopLabeling
import SemanticAnalysis.VariableResolution
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      ast <- lexerIO content >>= evalParseIO >>= resolveProgramIO >>= labelProgramIO
      let assembly = constructProgram (convertProgramWithFixedInstructions (genLProgram ast)) 
          outputFilePath = replaceExtension filePath "s"
      TIO.writeFile outputFilePath assembly
      putStrLn $ "Assembly written to " ++ outputFilePath
    _ -> putStrLn "Usage: hscc <file-path>"