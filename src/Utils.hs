module Utils
  ( Pretty (..),
    prettyPrint,
  )
where

import qualified Text.PrettyPrint as PP

-- | A typeclass for pretty printing
class Pretty a where
  -- | Pretty print a value
  pretty :: a -> PP.Doc

-- | Pretty print as a string
prettyPrint :: (Pretty a) => a -> String
prettyPrint = PP.render . pretty
