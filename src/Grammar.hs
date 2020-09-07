{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Grammar (
    Atom(..)
  , Sentence
  , Grammar
  , pp
) where

import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint as PP

data Atom = Chr Char | Eps | Var String
  deriving (Eq, Show, Ord)

class Pretty a where
  pretty :: a -> PP.Doc
  pp :: a -> String
  pp = PP.render . pretty

instance Pretty Atom where
  pretty (Chr c) = PP.char c
  pretty (Eps) = PP.text "\\e"
  pretty (Var s) = PP.text s

type Sentence = [Atom]

instance Pretty Sentence where
  pretty atoms = PP.hsep $ map pretty atoms

type Grammar = Map.Map String [Sentence]

instance Pretty Grammar where
  pretty prods = PP.sep $ PP.punctuate semi $ map worker $ Map.assocs prods
    where
      worker :: (String, [Sentence]) -> PP.Doc
      worker (lvar, alts) = PP.text lvar PP.<+> PP.text "::=" PP.<+> (PP.sep $ PP.punctuate alt (map pretty alts))

      alt = PP.text " |"
      semi = PP.text " ;"

