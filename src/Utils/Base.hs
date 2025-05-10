module Utils.Base (leftErrorIO) where

import System.Exit (exitFailure)

leftErrorIO :: (Show err) => (a -> Either err b) -> a -> IO b
leftErrorIO f a = case f a of
  Right b -> return b
  Left err -> do
    print err
    exitFailure