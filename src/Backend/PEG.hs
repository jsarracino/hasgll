module Backend.PEG (
    ParseResult(..)
  , recognize
  , matches
) where

import Grammar
import qualified Data.Map.Strict as Map

data ParseResult = NoParse | Partial {rem :: String} deriving (Eq, Show, Ord)

recognize :: Grammar -> String -> String -> ParseResult
recognize g start s = case foldl tryRecog NoParse ((Map.!) g start) of
  NoParse -> NoParse
  Partial s' -> if s == s' then NoParse else Partial s'
  where
    tryRecog :: ParseResult -> Sentence -> ParseResult
    tryRecog (Partial x) _ = Partial x
    tryRecog NoParse sent = foldl worker (Partial s) sent

    worker :: ParseResult -> Atom -> ParseResult
    worker NoParse _ = NoParse
    worker x@Partial{} Eps = x
    worker (Partial cs) (Chr c) = case cs of 
      []     -> NoParse
      c':cs' -> if c' == c then Partial cs' else NoParse
    worker (Partial cs) (Var v) = recognize g v cs

-- recogPartial :: Grammar -> String -> String -> Bool
-- recogPartial g start s 

-- fuzz :: Grammar -> String -> Int -> [String]

matches :: Grammar -> String -> String -> Bool
matches g start s = case recognize g start s of 
  NoParse -> False
  Partial{} -> True
