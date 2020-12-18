{-# LANGUAGE FlexibleInstances #-}

module Output.PEG (
    pretty
  , pp
) where

import Grammar hiding (pretty, pp)
import qualified Text.PrettyPrint as PP
import qualified Data.Map.Strict as Map

class PrettyPEG a where
  pretty :: a -> PP.Doc
  pp :: a -> String
  pp = PP.render . Output.PEG.pretty

instance PrettyPEG Grammar where
  pretty prods = PP.sep $ PP.punctuate semi $ map worker $ Map.assocs prods
    where
      worker :: (String, [Sentence]) -> PP.Doc
      worker (lvar, alts) = PP.text lvar PP.<+> PP.text "<-" PP.<+> (PP.sep $ PP.punctuate alt (map pretty alts))

      alt = PP.text " / "
      semi = PP.text " ;"

instance PrettyPEG Atom where
  pretty (Chr c) = PP.char c
  pretty Eps = PP.text "\\e"
  pretty (Var s) = PP.text s
  
instance PrettyPEG Sentence where
  pretty atoms = PP.hsep $ map pretty atoms