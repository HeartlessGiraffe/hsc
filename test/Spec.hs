import Control.Monad.State (evalStateT)
import Lexer.Lexer
import Parser.Parser
import Utils (prettyPrint)

evalParseExp :: Precedence -> CodeString -> Exp
evalParseExp p code = case lexer code of
  Right ts -> case evalStateT (parseExp p) ts of
    Right e -> e
    Left err' -> error (show err')
  Left err -> error (show err)

main :: IO ()
main = do
  let string1 = "1 * 2 - 3 * (4 + 5) + (-1)"
      string2 = "(3 + 2 / 3) * (1 % 2 - 1)"
  putStrLn $ prettyPrint (evalParseExp minimumPrecedence string1)
  putStrLn $ prettyPrint (evalParseExp minimumPrecedence string2)
