module Interpreter (
    initForest
  -- , growDeriv
  , growDerivEmpties
  , growDerivCallsite
  , checkSentenceConserv
  , checkSentenceExact
  , CallCache
  , Derivation
  , Forest
  , growSentences
  , sentences
  , parses
  , exceedsStr
) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Grammar ( Sentence, Grammar, Atom(..))

type CallCache = S.Set (String, Int)

type Derivation = (Sentence, CallCache)

growDerivCallsite :: String -> Grammar -> Int -> Derivation -> [Derivation]
growDerivCallsite str prods depth (sent, calls) = case sent of 
  [] -> [([], calls)]
  (Eps :sent') -> map (expand Eps) (growDerivCallsite str prods depth (sent', calls))
  (Chr c : sent') -> 
    case str of 
      [] -> []
      (c' : cs) -> 
        if c == c' 
          then map (expand (Chr c)) (growDerivCallsite cs prods (depth + 1) (sent', calls)) 
          else []
  (Var v :sent') -> 
    [(pref ++ sent', (v, depth) `S.insert` calls) | pref <- (M.!) prods v, checkSentenceConserv depth str (pref ++ sent', calls)]

type EmptyDeriv = (Sentence, S.Set String)

growDerivEmpties :: String -> Grammar -> EmptyDeriv -> [EmptyDeriv]
growDerivEmpties str prods (sent, empties) = case sent of 
  [] -> [([], empties)]
  (Eps :sent') -> map (expand Eps) (growDerivEmpties str prods (sent', empties))
  (Chr c : sent') -> 
    case str of 
      [] -> []
      (c' : cs) -> 
        if c == c' 
          then map (expand (Chr c)) (growDerivEmpties cs prods (sent', empties)) 
          else []
  (Var v :sent') -> 
    [ (pref ++ sent', (if hasEmpty pref then S.singleton v else S.empty) `S.union` empties) 
      | pref <- (M.!) prods v, 
        if S.member v empties then not $ hasEmpty pref else True,
        not $ exceedsStr str (pref ++ sent')
    ]

expand :: Atom -> (Sentence, a) -> (Sentence, a)
expand h (s, c) = (h : s, c)

hasEmpty :: Sentence -> Bool
hasEmpty (Eps : _) = True
hasEmpty _ = False

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
  ([], Eps : sent') -> checkSentenceExact [] sent'
  ([], _) -> False
  (_ : _, []) -> False
  (c : cs, Chr c' : sent') -> if c == c' then checkSentenceExact cs sent' else False
  (c : cs, Eps : sent') -> checkSentenceExact (c : cs) sent'
  (_, Var _ : _) -> False
  

emptyLength :: S.Set String -> Sentence -> Int
emptyLength empties sent = case sent of 
  [] -> 0
  Eps : tl -> emptyLength empties tl
  Chr c : tl -> 1 + emptyLength empties tl
  Var v : tl -> (if S.member v empties then 1 else 0) + emptyLength (v `S.insert` empties) tl

exceedsStr :: String -> Sentence -> Bool
exceedsStr str sent = 
  emptyLength S.empty sent > length str

-- type Forest = S.Set Derivation
type Forest = S.Set EmptyDeriv

growSentences :: String -> Grammar -> Forest -> Forest
growSentences str g states = S.fromList $ (S.toList states) >>= (growDerivEmpties str g)

sentences :: Forest -> [Sentence]
sentences states = map fst $ S.toList states

initForest :: String -> Grammar -> Forest
initForest v prods = S.fromList $ map (\s -> (s, S.empty)) $ (M.!) prods v

matches :: String -> Grammar -> Int -> Forest -> S.Set (EmptyDeriv, Int)
matches str g x forest = S.map (\m -> (m, x)) matchedSents `S.union` remainder
  where
    matchedSents = S.filter (checkSentenceExact str . fst) forest
    remainder = let nxt = growSentences str g forest in
      if nxt == forest then S.map (\m -> (m, x)) (S.filter (checkSentenceExact str . fst) forest) else matches str g (x + 1) nxt

-- partial
parses :: String -> Grammar -> String -> [(EmptyDeriv, Int)]
parses start g str = S.toList $ matches str g 0 (initForest start g)