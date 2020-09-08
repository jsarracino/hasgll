module Interpreter (
    initForest
  , growDeriv
  , checkSentenceConserv
  , checkSentenceExact
  , CallCache
  , Derivation
  , Forest
  , growSentences
  , sentences
  , parses
) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Grammar ( Sentence, Grammar, Atom(..))

type CallCache = S.Set (String, Int)

type Derivation = (Sentence, CallCache)

growDeriv :: String -> Grammar -> Int -> Derivation -> [Derivation]
growDeriv str prods depth (sent, calls) = case sent of 
  [] -> [([], calls)]
  (Eps :sent') -> map (expand Eps) (growDeriv str prods depth (sent', calls))
  (Chr c : sent') -> 
    case str of 
      [] -> []
      (c' : cs) -> 
        if c == c' 
          then map (expand (Chr c)) (growDeriv cs prods (depth + 1) (sent', calls)) 
          else []
  (Var v :sent') -> 
    [(pref ++ sent', (v, depth) `S.insert` calls) | pref <- (M.!) prods v, checkSentenceConserv depth str (pref ++ sent', calls)]

  where
    expand :: Atom -> Derivation -> Derivation
    expand h (s, c) = (h : s, c)

type Forest = S.Set Derivation

checkSentenceConserv :: Int -> String -> Derivation -> Bool
checkSentenceConserv _ [] ([], _) = True
checkSentenceConserv _ [] (_, _) = False
checkSentenceConserv _ _ ([], _) = False
checkSentenceConserv idx (c : cs) (sent, expanded) = case sent of 
  [] -> False
  (Eps : sent') -> checkSentenceConserv idx (c : cs) (sent', expanded)
  (Chr c' : sent') -> if c == c' then checkSentenceConserv (idx + 1) cs (sent', expanded) else False
  (Var v : _) -> not $ S.member (v, idx) expanded

checkSentenceExact :: String -> Sentence -> Bool
checkSentenceExact str sent = case (str, sent) of 
  ([], []) -> True
  (_, []) -> False
  (c : cs, Chr c' : sent') -> if c == c' then checkSentenceExact cs sent' else False
  (c : cs, Eps : sent') -> checkSentenceExact (c : cs) sent'
  (_, Var _ : _) -> False

growSentences :: String -> Grammar -> Forest -> Forest
growSentences str g states = S.fromList $ (S.toList states) >>= (growDeriv str g 0)

sentences :: Forest -> [Sentence]
sentences states = map fst $ S.toList states

initForest :: String -> Grammar -> Forest
initForest v prods = S.fromList $ map (\s -> (s, S.empty)) $ (M.!) prods v

matches :: String -> Grammar -> Int -> Forest -> S.Set (Derivation, Int)
matches str g x forest = S.map (\m -> (m, x)) matchedSents `S.union` remainder
  where
    matchedSents = S.filter (checkSentenceExact str . fst) forest
    remainder = let nxt = growSentences str g forest in
      if nxt == forest then S.map (\m -> (m, x)) forest else matches str g (x + 1) nxt

-- partial
parses :: String -> Grammar -> String -> [(Derivation, Int)]
parses start g str = S.toList $ matches str g 0 (initForest start g)