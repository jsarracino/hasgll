{-# LANGUAGE ScopedTypeVariables #-}

module Logic (
    subset
  , equality
  , fuzzDepth
  , replaceVars
  , fuzz
  , fuzzPEG
  , replaceVarsPEG
  , cfg2PEG
) where

import Grammar
import Backend.Earley ( buildEarley, recognize )
import Backend.PEG (recogFailPoint)
import Output.PEG
import Backend.Grampa
import qualified Text.Earley as E
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad
import qualified Backend.PEG as PEG
import Data.List
import Data.Maybe
import Data.List

import Logic.OrderPEG
import Z3.Monad

import System.IO.Unsafe


tokens :: Grammar -> [Char]
tokens g = Map.elems g >>= (\e -> e >>= (mapMaybe worker))
  where
    worker (Chr c) = Just c
    worker _ = Nothing

fuzz :: Grammar -> String -> [String]
fuzz g start = take 100000 $ fuzzDepth g start 3

takeTerms :: [Sentence] -> [Sentence]
takeTerms alts = filter (all isTerm) alts
  where
    isTerm (Var _) = False
    isTerm _ = True

replaceVars :: Map.Map String [String] -> [Sentence] -> [String]
replaceVars env sents = join $ map (foldl combine [[]]) sents
  where
    combine :: [String] -> Atom -> [String]
    combine ss nxt = [t ++ res | t <- ss, res <- worker nxt]

    worker (Var v) = (Map.!) env v
    worker Eps = []
    worker (Chr c) = [[c]]

-- prefixes :: String -> [String]
-- prefixes

replaceVarsPEG :: Map.Map String [String] -> Grammar -> String -> [String]
-- simple version, probably slower
-- replaceVarsPEG env g start = join $ map (\(l, r) -> filter (taker r) l) (zip (map (foldl (combine env) init) sents) [0..length sents])
--   where
--     combine :: Map.Map String [String] -> [String] -> Atom -> [String]
--     combine env ss nxt = [t ++ res | t <- ss, res <- worker env nxt]

--     restrict :: Grammar -> String -> Int -> Grammar
--     restrict gram name processed = Map.adjust (take processed) name gram

--     init :: [String]
--     init = [[]]
--     sents :: [Sentence]
--     sents = (Map.!) g start

--     taker :: Int -> String -> Bool
--     taker k s = let g' = restrict g start k in not $ any (PEG.matches g' start) (inits s)

--     -- handleSent :: []

--     worker env (Var v) = (Map.!) env v
--     worker _ Eps = []
--     worker _ (Chr c) = [[c]]
-- more complicated, probably faster version (filters earlier)
-- TODO: benchmark and compare these implementations
replaceVarsPEG env g start = join $ zipWith ($) (map (foldr (flip combine) init) sents) [0..length sents]
  where
    combine :: (Int -> [String]) -> Atom -> (Int -> [String])
    combine ssf nxt k = 
      let g' = restrict g start k in 
      let taker = \s -> not $ any (PEG.matches g' start) (inits s) in
      let cands = [t ++ res | t <- ssf k, res <- worker nxt] in
        filter taker cands

    restrict :: Grammar -> String -> Int -> Grammar
    restrict gram name processed = Map.adjust (take processed) name gram

    init :: Int -> [String]
    init _ = [[]]
    sents :: [Sentence]
    sents = (Map.!) g start

    -- handleSent :: []

    worker (Var v) = (Map.!) env v
    worker Eps = []
    worker (Chr c) = [[c]]

fuzzDepth :: Grammar -> String -> Int -> [String]
fuzzDepth g start n
  | n == 0  = replaceVars Map.empty $ takeTerms ((Map.!) g start)
  | n > 0   = 
    let prevs = Map.fromList $ map (\s -> (s, fuzzDepth g s (n-1))) (Map.keys g) in
      replaceVars prevs $ (Map.!) g start

  | n < 0   = error "bad depth argument to fuzzDepth"

-- fuzzDepthMemo :: Grammar -> String -> Int -> Map.Map (String, Int) [String] -> [String]
-- fuzzDepthMemo g start n memo 

fuzzPEG :: String -> Grammar -> Int -> [String]
fuzzPEG start g n 
  | n == 0  = replaceVarsPEG (Map.map (\_ -> []) g) g start
  | n > 0   = 
    let prevs = Map.fromList $ map (\s -> (s, fuzzPEG s g (n-1))) (Map.keys g) in
      replaceVarsPEG prevs g start

  | n < 0   = error "bad depth argument to fuzzPEG"

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

type FuzzConfig = (Int, Int)
type SearchCache = [Int]

fuzzWithConfig :: Grammar -> String -> FuzzConfig -> [String]
fuzzWithConfig g s (amount, depth) = fuzzDepth g s depth

readOrd :: String -> Maybe Ordering
readOrd = read

checkOneProd :: Grammar -> String -> FuzzConfig -> [(Int, String)]
checkOneProd g start config = let good = fuzzWithConfig g start config in 
  catMaybes $ map (recogFailPoint g start) good 

completeProduction :: Grammar -> String -> IO (Maybe Grammar)
completeProduction g p = let ordW = foldM worker Map.empty (genCounters g) in do
  ord <- ordW
  (_, out) <- evalZ3 $ rewriteGram g ord

  case out of 
    Just out' -> if null $ genCounters out' then completeProduction out' p else pure $ Just out'
    Nothing   -> pure $ Nothing
  

  where
    worker :: PickleOrd -> (Int, String) -> IO PickleOrd
    worker ord (fp, cx) = do 
      putStrLn $ "Found failpoint " ++ show fp ++ " on example " ++ show cx 
      putStrLn $ "Grammar is " ++ pp g
      putStrLn $ "Please give an ordering for " ++ show lhs ++ " compared to " ++ show rhs
      nxtO <- readOrd <$> getLine
      pure $ Map.insert lhs (Map.insert rhs nxtO rmap) ord

      where

        lhs = (Map.!) g p !! fp
        rhs = (Map.!) g p !! (fp + 1)
        rmap = case Map.lookup lhs ord of
          Just t -> t
          Nothing -> Map.empty
    cnfg = (1000000,4)

    genCounters gram = (nubBy (\l r -> fst l == fst r) $ checkOneProd gram p cnfg)



cfg2PEG :: Grammar -> [String] -> IO (Maybe Grammar)
cfg2PEG g completed = if length completed == length (Map.keys g) then pure $ Just g else do 
  nxtG <- completeProduction g nxtProd
  case nxtG of 
    Just g' -> cfg2PEG g' (nxtProd : completed)
    Nothing -> putStrLn "error: z3 query failed" >> pure Nothing
  where
    nxtProd = head $ Map.keys g \\ completed

