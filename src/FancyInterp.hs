module FancyInterp (
    parses
  , growDerivFancy
  , initForest
  , growSentences
  , ExtAtom(..)
  , exceedsStr
  , strictLength
  , relaxedLength
)
where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Grammar ( Sentence, Grammar, Atom(..))

data ExtAtom = Atm Atom | Pop String deriving (Eq, Show, Ord)

type CallCache = ([String], S.Set String)

type Derivation = ([ExtAtom], CallCache)

growDerivFancy :: String -> Grammar -> Derivation -> [Derivation]
growDerivFancy str prods (sent, (calls, empties)) = case sent of 
  [] -> [([], (calls, empties))]
  (Pop v: sent') -> case calls of 
    c : calls' -> if c == v then growDerivFancy str prods (sent', (calls', empties `S.intersection` (S.fromList calls'))) else error "inconceivable"
  (Atm Eps :sent') -> growDerivFancy str prods (sent', (calls, empties))
  (Atm (Chr c) : sent') -> 
    case str of 
      [] -> []
      (c' : cs) -> 
        if c == c' 
          then map (expand (Chr c)) (growDerivFancy cs prods (sent', (calls, empties))) 
          else []
  (Atm (Var v) :sent') -> 
    [ (map Atm pref ++ Pop v : sent', (v : calls, (if hasEmpty pref then S.singleton v else S.empty) `S.union` empties)) 
      | pref <- (M.!) prods v, 
        if S.member v empties then not $ hasEmpty pref else True,
        not $ exceedsStr str empties pref sent'
        -- True
    ]

expand :: Atom -> ([ExtAtom], a) -> ([ExtAtom], a)
expand h (s, c) = (Atm h : s, c)

hasEmpty :: Sentence -> Bool
hasEmpty (Eps : _) = True
hasEmpty _ = False


checkSentenceExact :: String -> [ExtAtom] -> Bool
checkSentenceExact str sent = case (str, sent) of 
  ([], []) -> True
  ([], Atm Eps : sent') -> checkSentenceExact [] sent'
  ([], _) -> False
  (_ : _, []) -> False
  (c : cs, Atm (Chr c') : sent') -> if c == c' then checkSentenceExact cs sent' else False
  (c : cs, Atm Eps : sent') -> checkSentenceExact (c : cs) sent'
  (_, Atm _ : _) -> False
  (_, Pop _ : _) -> False
  

strictLength :: S.Set String -> Sentence -> Int
strictLength empties sent = case sent of 
  [] -> 0
  Eps : tl -> strictLength empties tl
  Chr c : tl -> 1 + strictLength empties tl
  Var v : tl -> (if S.member v empties then 1 else 0) + strictLength empties tl

relaxedLength :: [ExtAtom] -> Int
relaxedLength esent = case esent of 
  Atm (Chr c) : tl -> 1 + relaxedLength tl
  _ : tl -> relaxedLength tl
  [] -> 0

exceedsStr :: String -> S.Set String -> Sentence -> [ExtAtom] -> Bool
exceedsStr str empties pref suff = 
  strictLength empties pref + relaxedLength suff > length str

type Forest = S.Set Derivation

growSentences :: String -> Grammar -> Forest -> Forest
growSentences str g states = S.fromList $ (S.toList states) >>= (growDerivFancy str g)

-- sentences :: Forest -> [Sentence]
-- sentences states = map fst $ S.toList states

initForest :: String -> Grammar -> Forest
initForest v prods = S.fromList $ map (\s -> (map Atm s, ([], S.empty))) $ (M.!) prods v

matches :: String -> Grammar -> Int -> Forest -> S.Set (Derivation, Int)
matches str g x forest = S.map (\m -> (m, x)) matchedSents `S.union` remainder
  where
    matchedSents = S.filter (checkSentenceExact str . fst) forest
    remainder = let nxt = growSentences str g forest in
      if nxt == forest then S.map (\m -> (m, x)) (S.filter (checkSentenceExact str . fst) forest) else matches str g (x + 1) nxt

-- partial
parses :: String -> Grammar -> String -> [(Derivation, Int)]
parses start g str = S.toList $ matches str g 0 (initForest start g)