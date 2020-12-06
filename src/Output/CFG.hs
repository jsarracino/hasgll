{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Output.CFG where

import Grammar
import qualified Text.PrettyPrint as PP
import qualified Data.Map.Strict as Map

instance Pretty Grammar where
  pretty prods = PP.sep $ PP.punctuate semi $ map worker $ Map.assocs prods
    where
      worker :: (String, [Sentence]) -> PP.Doc
      worker (lvar, alts) = PP.text lvar PP.<+> PP.text "::=" PP.<+> (PP.sep $ PP.punctuate alt (map pretty alts))

      alt = PP.text " |"
      semi = PP.text " ;"

instance Pretty Atom where
  pretty (Chr c) = PP.char c
  pretty (Eps) = PP.text "\\e"
  pretty (Var s) = PP.text s
  
instance Pretty Sentence where
  pretty atoms = PP.hsep $ map pretty atoms