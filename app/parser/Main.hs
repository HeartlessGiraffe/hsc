module Main (main) where

import Lexer.Lexer (lexerIO)
import Parser.Parser (evalParseIO)
import System.Environment (getArgs)
import Utils.Pretty (prettyPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      ast <- lexerIO content >>= evalParseIO
      putStrLn $ prettyPrint ast
    _ -> putStrLn "Usage: hsc-parser <file-path>"
