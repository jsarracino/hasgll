{-# LANGUAGE ScopedTypeVariables #-}

module Logic (
    subset
  , equality
  , fuzzDepth
  , replaceVars
  , fuzz
  , fuzzPEG
  , substProd
  , fuzzAllProds
  , fuzzProd
  , replaceVarsPEG
  , cfg2PEG
  , getCounterExs
  , compileToPEG
  , compileOneProd
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
tokens g = Map.elems g >>= (\e -> e >>= mapMaybe worker)
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
defaultConfig :: FuzzConfig
defaultConfig = (10000,6)
type SearchCache = [Int]

fuzzWithConfig :: Grammar -> String -> FuzzConfig -> [String]
fuzzWithConfig g s (amount, depth) = take amount $ fuzzDepth g s depth

readOrd :: String -> Maybe Ordering
readOrd = read

checkOneProd :: Grammar -> String -> FuzzConfig -> [(Int, String)]
checkOneProd g start config = let good = fuzzWithConfig g start config in 
  mapMaybe (recogFailPoint g start) good 

completeProduction :: Grammar -> String -> IO (Maybe Grammar)
completeProduction g p = let ordW = foldM worker Map.empty (genCounters g) in do
  ord <- ordW
  (_, out) <- evalZ3 $ rewriteGram g ord

  case out of 
    Just out' -> if null $ genCounters out' then completeProduction out' p else pure $ Just out'
    Nothing   -> pure Nothing
  

  where
    worker :: PickleOrd -> (Int, String) -> IO PickleOrd
    worker ord (fp, cx) = do 
      putStrLn $ "Found failpoint " ++ show fp ++ " on example " ++ show cx 
      -- putStrLn $ "Grammar is " ++ pp g
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



substProd :: Grammar -> String -> Int -> (String, Grammar)
substProd gram prod idx = (newProd, gram'')
  where
    newProd = "_" ++ prod ++ "_" ++ show idx
    alts = (Map.!) gram prod
    it = alts !! idx
    rhs = Var newProd
    (pref, suf) = splitAt idx alts
    alts' = pref ++ [[rhs]] ++ drop 1 suf
    gram' = Map.insert newProd [it] gram
    gram'' = Map.insert prod alts' gram'

fuzzProd :: Grammar -> String -> Int -> [String]
fuzzProd gram prod idx = fuzzWithConfig gram' newProd config 
  where
    (newProd, gram') = substProd gram prod idx
    config = (10000, 5)

fuzzAllProds :: Grammar -> Map.Map Sentence [String]
fuzzAllProds gram = Map.fromList $ map worker confs
  where
    confs = [(lhs, (Map.!) gram lhs !! idx, idx) | lhs <- Map.keys gram, idx <- [0..length ((Map.!) gram lhs) - 1]] 
    worker (prod, sent, idx) = (sent, fuzzProd gram prod idx)

-- convert cx to ordering

mapMaybeWithKey :: (Ord k) => (k -> a -> Maybe b) -> Map.Map k a -> Map.Map k b
mapMaybeWithKey f mp = Map.foldlWithKey worker Map.empty mp
  where
    worker acc l x = case f l x of 
      Just y -> Map.insert l y acc
      Nothing -> acc

getCounterExs :: Grammar -> Map.Map Sentence [String] -> Map.Map String [(Sentence, [Sentence])]
getCounterExs gram spec = Map.mapWithKey worker gram
  where
    worker :: String -> [Sentence] -> [(Sentence, [Sentence])]
    worker lhs alts = catMaybes $ map (\x -> (\(y, i) -> (y, take i alts)) <$> checkAlt lhs x) (zip alts [0..])

    checkAlt :: String -> (Sentence, Int) -> Maybe (Sentence, Int)
    checkAlt lhs (s, idx) = 
      if any (not . PEG.matches gram lhs) $ (Map.!) spec s then Just (s, idx) else Nothing

integrateCX :: Grammar -> String -> [(Sentence, [Sentence])] -> Z3 (Maybe Grammar)
integrateCX gram lhs cxs = do
  (_, rhs) <- orderAltsMany cxs $ (Map.!) gram lhs 
  case rhs of 
    Just rhs' -> pure $ Just $ Map.insert lhs rhs' gram
    Nothing -> pure Nothing

filterCX :: Map.Map Sentence [String] -> Maybe Grammar -> Maybe Grammar
filterCX spec = mfilter worker
  where
    worker :: Grammar -> Bool
    worker g = let larms = Map.toList g in 
       all (checkLarms g) larms
    checkLarms :: Grammar -> (String, [Sentence]) -> Bool
    checkLarms g (lhs, alts) = all (all (PEG.matches g lhs) . (Map.!) spec) alts

compileOneProd :: Grammar -> String -> Map.Map Sentence [String] -> [[(Sentence, [Sentence])]] -> Z3 (Maybe Grammar)
compileOneProd _ _ _ [] = pure Nothing
compileOneProd gram lhs spec (cand:cxs) = do 
  it <- integrateCX gram lhs cand 
  (result, _) <- solverCheckAndGetModel  
  case result of 
    Sat -> case filterCX spec it of 
      Nothing -> case Map.lookup lhs (getCounterExs gram spec) of
        Nothing   -> compileOneProd gram lhs spec cxs
        Just cxs' -> compileOneProd gram lhs spec $ cxs ++ [t:cand | t <- cxs'] 
      Just gram' -> pure $ Just gram'
    _ -> compileOneProd gram lhs spec cxs


-- filter
compileToPEG :: Grammar -> Z3 (Maybe Grammar)
compileToPEG gram = worker [[]]
  where
    spec = fuzzAllProds gram



    -- take all the possible counterexamples and iterate until we get an answer
    worker :: [[(String, [(Sentence, [Sentence])])]] -> Z3 (Maybe Grammar)
    worker cxss = error "todo"
      -- gs <- mapM (integrateCX gram 
    -- worker cxss = case getCounterExs g spec of 
    --   Just (lhs, it, prefs) -> do 
    --     (_, x) <- orderAltsMany ((it, prefs):cxs) ((Map.!) g lhs)
    --     case x of 
    --       Just rhs' -> worker (Map.insert lhs rhs' g) ((it, prefs):cxs)
    --       Nothing -> pure $ Right (g, cxs)
    --   Nothing -> pure $ Right (g, cxs)
