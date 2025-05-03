module Main (main) where

import AssemblyGen.AssemblyGen (convertProgramWithFixedInstructions)
import Lexer.Lexer (lexer)
import Parser.Parser (evalParse)
import TACKY.TACKY
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Emission.Emission
import System.FilePath (replaceExtension)
import SemanticAnalysis.VariableResolution

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      case lexer content of
        Right tokens -> case evalParse tokens of
          Right ast' -> case resolveProgram ast' of 
            Right ast -> do
              let assembly = constructProgram (convertProgramWithFixedInstructions (genProgram ast))
              let outputFilePath = replaceExtension filePath "s"
              writeFile outputFilePath assembly
              putStrLn $ "Assembly written to " ++ outputFilePath
            Left err -> do 
              print err 
              exitFailure
          Left err -> do
            print err
            exitFailure
        Left err -> do
          print err
          exitFailure
    _ -> putStrLn "Usage: hscc <file-path>"