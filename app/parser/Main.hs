module Main (main) where

import Lexer.Lexer (lexer)
import Parser.Parser (evalParse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Utils (prettyPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      case lexer content of
        Right tokens -> case evalParse tokens of
          Right ast -> putStrLn $ prettyPrint ast
          Left err -> do
            print err
            exitFailure
        Left err -> do
          print err
          exitFailure
    _ -> putStrLn "Usage: hsc-parser <file-path>"
