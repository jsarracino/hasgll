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

subset :: String -> Grammar -> Grammar -> Bool
subset start l r = all (\s -> recognize s l start && recognize s r start) $ fuzz l start ++ fuzz r start

equality :: String -> Grammar -> Grammar -> Bool
equality start l r = subset start l r && subset start r l