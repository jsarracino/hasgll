module Grammar (
    Atom(..)
  , Sentence
  , Grammar
  , pp
  , Pretty
  , pretty
) where

import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint as PP

data Atom = Chr Char | Eps | Var String
  deriving (Eq, Show, Ord)

class Pretty a where
  pretty :: a -> PP.Doc
  pp :: a -> String
  pp = PP.render . pretty

type Grammar = Map.Map String [Sentence]

type Sentence = [Atom]
