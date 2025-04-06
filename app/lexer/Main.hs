module Main (main) where

import Lexer.Lexer (lexer)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- 一个命令行程序，从文件读取代码文本，并将其传递给词法分析器
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      case lexer content of
        Right tokens -> print tokens
        Left err -> do
          print err
          exitFailure
    _ -> putStrLn "Usage: lexer <file-path>"
