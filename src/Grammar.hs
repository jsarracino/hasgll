module Grammar (
    Atom(..)
  , Sentence
  , Grammar
) where

import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint as PP

data Atom = Chr Char | Eps | Var String
  deriving (Eq, Show, Ord)

type Sentence = [Atom]

type Grammar = Map.Map String [Sentence]

