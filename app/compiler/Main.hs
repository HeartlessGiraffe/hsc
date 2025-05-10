module Main (main) where

import AssemblyGen.AssemblyGen (convertProgramWithFixedInstructions)
import qualified Data.Text.IO as TIO
import Emission.Emission
import Lexer.Lexer (lexerIO)
import Parser.Parser (evalParseIO)
import SemanticAnalysis.LoopLabeling
import SemanticAnalysis.VariableResolution
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import TACKY.TACKY

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