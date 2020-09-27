

module Backend.ANTLR (
    exampleGram
  , example
) where

import Text.ANTLR.Grammar
import Text.ANTLR.LR
import Text.ANTLR.Set
import qualified Grammar as Gen

-- Digit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 ;
-- Num ::= Digit Num | Digit ;
-- Expr ::= Expr + Expr | Expr * Expr | Expr - Expr | Expr / Expr | Num

production :: nts -> (ProdRHS s nts ts) -> Production s nts ts dt
production lhs rhs = Production lhs rhs Nothing

exampleGram :: Grammar () String String ()
exampleGram = (defaultGrammar "Foo" :: Grammar () String String Bool)
  { ns = fromList ["E"]
  , ts = fromList ["1", "+", "*"]
  , s0 = "E"
  , ps =
    [ production "E" $ Prod Pass [T "1"]
    , production "E" $ Prod Pass [NT "E", T "+", NT "E"]
    , production "E" $ Prod Pass [NT "E", T "*", NT "E"]
    ]
  }

example = glrParse exampleGram (\_ -> True) ["1", "*", "1"]