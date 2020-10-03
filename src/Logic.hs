module Logic (
    subset
  , equality
) where

import Grammar
import Backend.Earley ( buildEarley, recognize )
import qualified Text.Earley as E
import qualified Data.Map.Strict as Map
import Data.Maybe

tokens :: Grammar -> [Char]
tokens g = Map.elems g >>= (\e -> e >>= (mapMaybe worker))
  where
    worker (Chr c) = Just c
    worker _ = Nothing

fuzz :: Grammar -> String -> [String]
fuzz g start =  map snd $ take 1000 $ E.language (E.generator (buildEarley g start) (tokens g))

-- compare two grammars for a subset relation, and return:
  -- Left () if the LHS appears to be a subset of the RHS
  -- Right cxs if the LHS is NOT a subset and cxs are witnesses
subset :: String -> Grammar -> Grammar -> Either () [String]
subset start l r = foldl combiner (Left ()) (map recognizer examples)
  where
    combiner (Left ()) (Left ()) = Left ()
    combiner (Left ()) (Right s) = Right [s]
    combiner (Right cxs) (Left ()) = Right cxs
    combiner (Right cxs) (Right s) = Right $ s : cxs
    recognizer s = if recognize s l start then Left () else Right s
    examples = fuzz r start

equality :: String -> Grammar -> Grammar -> Either () ([String], [String])
equality start l r = combiner (subset start l r) (subset start r l)
  where
    combiner (Left ()) (Left ()) = Left ()
    combiner (Right cxs) (Left ()) = Right (cxs, [])
    combiner (Left ()) (Right cxs) = Right ([], cxs)
    combiner (Right cxs) (Right cxs') = Right (cxs, cxs')