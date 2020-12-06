module Logic.OrderPEG  (
    comparable
  , orderAltsOne
  , orderAltsMany
  , fromMP
  , rewriteGram
  , GrammarOrdering
  , PickleOrd
  , script
  , showQuery
  , script1
  , script2
) where

import Grammar
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as BM

-- import Z3.Base
import Z3.Monad
import Data.List

type GrammarOrdering = Sentence -> Sentence -> Maybe Ordering

comparable :: GrammarOrdering -> Sentence -> Sentence -> Bool
comparable ord l r = case ord l r of 
  Just _ -> True
  _      -> False

type PickleOrd = Map.Map Sentence (Map.Map Sentence (Maybe Ordering))

fromMP :: PickleOrd -> GrammarOrdering
fromMP mp l r = case Map.lookup l mp of 
  Just mp' -> case Map.lookup r mp' of 
    Just it -> it
    Nothing -> error "missing rhs in inner ordering map"
  Nothing -> error "missing lhs in outer ordering map"

type Names = BM.Bimap Sentence String

-- instance (Ord a, Ord b) => Traversable (BM.Bimap a b) where

mkVars :: [String] -> Z3 [AST]
mkVars vs = mapM mkFreshIntVar vs

getVars :: [String] -> Z3 [AST]
getVars vs = mapM worker vs
  where
    worker v = mkStringSymbol v >>= mkIntVar 

points :: Eq a => [a] -> [(a, [a])]
points xs = map worker xs
  where
    worker t = (t, delete t xs)

addBounds :: Int -> [AST] -> Z3 ()
addBounds upper vs = do
  lowerV <- mkIntNum 0
  upperV <- mkIntNum upper
  forM_ vs (\x -> mkLe x upperV >>= assert)
  forM_ vs (\x -> mkLe lowerV x >>= assert)
  forM_ (points vs) (\(x, xs) -> forM_ xs ((\t -> mkEq x t >>= mkNot >>= assert)))

script1 = do
  vsAST <- mkVars ["x", "y", "z"]
  addBounds (length vsAST) vsAST
  withModel (\m -> mapM (evalInt m) vsAST)

script2 = do 
  script1
  vsAST <- getVars ["x", "y", "z"]
  withModel (\m -> mapM (evalInt m) vsAST)
  

script :: Z3 (Result, Maybe Model)
script = do 
  x <- mkFreshIntVar "x"
  z <- mkFreshIntVar "x"
  y <- mkFreshIntVar "y"
  lower <- mkIntNum 0
  upper <- mkIntNum 10
  mkLe lower x >>= assert 
  mkLe lower y >>= assert 
  mkLe x upper >>= assert 
  mkLe y upper >>= assert 
  mkEq x y >>= mkNot >>= assert
  mkEq z y >>= assert
  getModel

showQuery :: Z3 (Result, Maybe Model) -> IO String
showQuery query = do 
  res <- evalZ3 query
  case res of 
    (Sat, Just m) -> evalZ3 $ modelToString m
    _ -> pure "unsat/missing model"


makeOrderExpr :: Names -> Map.Map String AST -> Sentence -> Sentence -> Ordering -> Z3 AST
makeOrderExpr names z3vars l r it = constr l'' r''
  where
    l' = (BM.!) names l
    r' = (BM.!) names r
    l'' = (Map.!) z3vars l'
    r'' = (Map.!) z3vars r'
    constr = case it of 
      LT -> mkLt
      GT -> mkGt
      EQ -> mkEq

walkPickle :: Names -> Map.Map String AST -> PickleOrd -> Z3 [AST]
walkPickle names z3vars ord = Map.foldlWithKey worker (pure []) ord
  where
    worker :: Z3 [AST] -> Sentence -> Map.Map Sentence (Maybe Ordering) -> Z3 [AST]
    worker acc l rs = Map.foldlWithKey (biworker l) acc rs
    biworker :: Sentence -> Z3 [AST] -> Sentence -> Maybe Ordering -> Z3 [AST]
    biworker l acc r ord' = case ord' of 
      Nothing -> acc
      Just op -> do 
        pref <- acc
        v <- handleOp op (s2A l) (s2A r)
        pure $ pref ++ [v]
    
    handleOp LT = mkLt
    handleOp GT = mkGt
    handleOp EQ = mkEq

    s2A :: Sentence -> AST
    s2A sent = case BM.lookup sent names of 
      Just x -> (Map.!) z3vars x
      Nothing -> error $ "missing BM name " ++ show sent ++ " in " ++ show names
     
orderAltsOne :: PickleOrd -> [Sentence] -> Z3 (Result, Maybe [Sentence])
orderAltsOne ord alts = worker
  where
    names = zipWith (\s x -> (s, "var" ++ show x)) alts [0..]
    namesBM = BM.fromAList names
    name2Ast asts = Map.fromList $ zip (BM.keysR namesBM) asts

    worker = do
      asts <- mkVars $ BM.keysR namesBM
      addBounds (length alts) asts
      constraints <- walkPickle namesBM (name2Ast asts) ord
      forM_ constraints assert
      withModel (buildAlts (name2Ast asts))
      
    buildAlts :: Map.Map String AST -> Model -> Z3 [Sentence]
    buildAlts asts m = do
      mp <- mapM (\sent -> takeJust <$> evalInt m ((Map.!) asts sent)) (Map.fromList $ BM.toList namesBM)
      pure $ sortBy (orderAlts mp) alts
    
    takeJust (Just x) = x

    orderAlts :: Map.Map Sentence Integer -> Sentence -> Sentence -> Ordering
    orderAlts vals l r = compare ((Map.!) vals l) ((Map.!) vals r) 

orderAltsMany :: [(Sentence, [Sentence])] -> [Sentence] -> Z3 (Result, Maybe [Sentence])
orderAltsMany ords alts = worker
  where
    names = zipWith (\s x -> (s, "var" ++ show x)) alts [0..]
    namesBM = BM.fromAList names
    name2Ast asts = Map.fromList $ zip (BM.keysR namesBM) asts

    worker = do
      asts <- mkVars $ BM.keysR namesBM
      addBounds (length alts) asts
      constraintss <- forM (map pickleDisjunct ords) $ walkPickle namesBM (name2Ast asts)
      makeDisjunct constraintss >>= assert
      withModel (buildAlts (name2Ast asts))
      
    buildAlts :: Map.Map String AST -> Model -> Z3 [Sentence]
    buildAlts asts m = do
      mp <- mapM (\sent -> takeJust <$> evalInt m ((Map.!) asts sent)) (Map.fromList $ BM.toList namesBM)
      pure $ sortBy (orderAlts mp) alts
    
    takeJust (Just x) = x

    makeDisjunct :: [[AST]] -> Z3 AST
    makeDisjunct disjuncts = mapM orAct disjuncts >>= andAct
      where
        andAct arms = do
          tt <- mkTrue
          mkAnd $ [tt, tt] ++ arms
        orAct arms = mkOr arms

    pickleDisjunct :: (Sentence, [Sentence]) -> PickleOrd
    pickleDisjunct (rhs, lhss) = Map.singleton rhs $ Map.fromList orders
      where
        orders = map (\lhs -> (lhs, Just LT)) lhss

    orderAlts :: Map.Map Sentence Integer -> Sentence -> Sentence -> Ordering
    orderAlts vals l r = compare ((Map.!) vals l) ((Map.!) vals r) 



rewriteGram :: Grammar -> PickleOrd -> Z3 (Result, Maybe Grammar)
rewriteGram gram mp = do
  results <- mapM (orderAltsOne mp) gram
  pure $ Map.foldlWithKey builder (Sat, Just Map.empty) results
  
    where
      builder (Sat, Just mp') k (Sat, Just alts') = (Sat, Just $ Map.insert k alts' mp')
      builder x _ _ = x

-- rewriteWithCXs :: Grammar -> [(Sentence, [Sentence])] -> Z3 (Maybe Grammar)

-- * check the spec. if there exists a counterexample, then it must be the case that 
--   the corresponding production conflicts with the earlier prods.
--   convert this counterexample into an ordering predicate:
--     [a b c Conflict e f g]
--     => Conflict < a \/ Conflict < b \/ Conflict < c

-- convertCX :: [Sentence] -> Sentence -> PickleOrd
-- convertCX prios rhs = Map.fromList []

