module Main (main) where

import Lexer.Lexer (lexerIO)
import System.Environment (getArgs)

-- 一个命令行程序，从文件读取代码文本，并将其传递给词法分析器
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      ts <- lexerIO content
      print ts
    _ -> putStrLn "Usage: hsc-lexer <file-path>"
