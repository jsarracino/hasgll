module Logic.Schedule (
  scheduleDeps,
  rebuildDeps,
  scheduleGram
) where

import Z3.Monad
import Control.Monad

import Data.List

import Grammar

import qualified Data.Map.Strict as Map

mkVars :: [String] -> Z3 (Map.Map String AST)
mkVars lhss = Map.fromList <$> mapM (\s -> do v <- mkFreshIntVar s ; pure (s, v)) lhss

addBounds :: Int -> AST -> [AST] -> Z3 ()
addBounds upper lv vs = do
  lowerV <- mkIntNum 0
  upperV <- mkIntNum upper
  -- vs <- mkVars rhss
  -- lv <- mkFreshIntVar lhs
  forM_ (lv:vs) (\x -> mkLe x upperV >>= assert)
  forM_ (lv:vs) (\x -> mkLe lowerV x >>= assert)
  forM_ vs (\x -> mkLe x lv >>= assert)

getOrdering :: [(String, AST)] -> Z3 (Maybe (Map.Map String Integer))
getOrdering lhss = do 
  (r, m) <- getModel 
  case (r, m) of 
    (Sat, Just m') -> Just . Map.fromList <$> mapM (uncurry (worker m')) lhss
    _ -> pure Nothing

  where
    worker :: Model -> String -> AST -> Z3 (String, Integer)
    worker m s v = do 
      i <- evalInt m v
      case i of 
        Just x -> pure (s, x)
        Nothing -> fail "error in evalInt"


points :: Eq a => [a] -> [(a, [a])]
points xs = map worker xs
  where
    worker t = (t, delete t xs)

scheduleDeps :: Map.Map String [String] -> Z3 (Maybe (Map.Map String Integer))
scheduleDeps deps = do
    vs <- mkVars lhss 
    forM_ (Map.toList vs) (\(s, vs') -> addBounds (length lhss) ((Map.!) vs s) (getVS ((Map.!) deps s) vs))
    forM_ (points (Map.elems vs)) (\(x, xs) -> forM_ xs ((\t -> mkEq x t >>= mkNot >>= assert)))
    getOrdering (Map.toList vs)
  where
    lhss = Map.keys deps

    getVS :: [String] -> Map.Map String AST -> [AST]
    getVS ss vs = map ((Map.!) vs) ss

rebuildDeps :: [String] -> Map.Map String Integer -> [String]
rebuildDeps ss idxs = sortBy worker ss
  where
    worker l r = compare ((Map.!) idxs l) ((Map.!) idxs r)


scheduleGram :: Grammar -> Z3 (Maybe [String])
scheduleGram gram = do
  idxs <- scheduleDeps deps
  case idxs of 
    Just idxs' -> pure $ Just $ rebuildDeps (Map.keys gram) idxs'
    Nothing -> pure Nothing
  where
    deps = Map.map worker gram
    worker :: [Sentence] -> [String]
    worker sents = nub . concat $ fmap (foldl takeVars []) sents

    takeVars :: [String] -> Atom -> [String]
    takeVars acc a = case a of 
      Var v -> v : acc
      _ -> acc