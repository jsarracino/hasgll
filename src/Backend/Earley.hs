{-# LANGUAGE RecursiveDo, RankNTypes, OverloadedStrings, FlexibleContexts #-}


module Backend.Earley (
  buildEarley,
  recognize,
  example,
  openexample
)
  where 

import qualified Grammar as Gen
import Text.Earley
import Text.Earley.Grammar
import Control.Applicative
import qualified Data.ListLike as LL
import qualified Data.Map.Lazy as Map
import Control.Monad ( join, foldM )
import Control.Monad.Fix ( MonadFix(mfix) )


-- Digit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 ;
-- Num ::= Digit Num | Digit ;
-- Expr ::= Expr + Expr | Expr * Expr | Expr - Expr | Expr / Expr | Num

-- lookup' xs k = case lookup k xs of 
--   (Just x) -> x
--   Nothing -> error "whack"

type LazyMap k v = k -> v

lookup' :: (Eq k) => k -> LazyMap k v -> v
lookup' k mp = mp k

empty' :: LazyMap k v
empty' _ = error "whack"

assoc :: Eq k => k -> v -> LazyMap k v -> LazyMap k v
assoc k v f k' = if k == k' then v else f k'

single :: Eq k => k -> v -> LazyMap k v
single k v = assoc k v empty'


example :: Grammar r (Prod r e String String)
-- example = 
--   mdo 
--   x1   <- rule $ (++) <$> expr <*> ("+" *> expr)
--   x2   <- rule $ (++) <$> expr <*> ("*" *> expr)
--   expr <- rule $ x1 <|> x2 <|> "1"
--   pure expr
example = (\ ~e -> lookup' "E" e) <$> (mfix func)
  where 
    func ~e = let expr = lookup' "E" e in 
                        do
                            x1'   <- rule $ (++) <$> expr <*> ("+" *> expr)
                            x2'   <- rule $ (++) <$>  expr <*> ("*" *> expr)
                            expr' <- rule $ "1" <|> lookup' "x1" e <|> lookup' "x2" e
                            pure $ assoc "E" expr' $ assoc "x1" x1' $ assoc "x2" x2' $ empty'
-- example = rule $ base <|> recur
--   where
--     base = (Terminal matcher (pure f))
--     matcher "1" = Just "1"
--     f = id

--     recur = NonTerminal it g
--       -- mdo 
--       -- it <- (NonTerminal it g)
--       -- pure it
--     it = '5'
--     g = pure (:[])
  
  -- mdo 
  -- x1   <- rule $ (++) <$> expr <*> ("+" *> expr)
  -- x2   <- rule $ (++) <$> expr <*> ("*" *> expr)
  -- expr <- rule $ x1 <|> x2 <|> "1"
  -- pure expr

ex1 envs = rule $ (++) <$> (lookup' "E" envs) <*> ("+" *> (lookup' "E" envs))
-- exe :: LazyMap String (Prod r e Char String) -> Grammar r (Prod r e Char String)
exe envs = rule $ (lookup' "x1" envs) <|> "1"

-- openexample :: Grammar r (Prod r e Char String)
openexample = (\ ~e -> lookup' "E" e) <$> (mfix func)
  where
    func ~e = foldM (\e' (name, rle) -> do ret <- rle e'; pure $ assoc name ret e') e [("E", exe), ("x1", ex1)]
      
      -- (do expr <- exe e; pure (assoc "E" expr e)) >>= (\e' -> do x1 <- ex1 e'; pure (assoc "x1" x1 e'))





type Env r e = LazyMap String (Prod r e Char String)
type MEnv r e = LazyMap String (Grammar r (Prod r e Char String))

-- interpProds :: Env r e -> String -> [Gen.Sentence] -> 
interpProds env lhs alts = (lhs, rule $ foldl (<|>) empty (map worker alts))
  where
    -- worker :: Gen.Sentence -> Prod r e Char String
    worker = foldl (\acc atom -> (++) <$> acc <*> atom2prod atom) (atom2prod Gen.Eps)

    -- atom2prod :: Gen.Atom -> Prod r e Char String
    atom2prod (Gen.Chr c) = terminal (\c' -> if c' == c then Just [c] else Nothing)
    atom2prod Gen.Eps = pure []
    atom2prod (Gen.Var v) = lookup' v env

-- interpEarley :: Env r e -> (forall r. Grammar r (Prod r e t a))



buildEarley :: Gen.Grammar -> String -> (forall r. Grammar r (Prod r e Char String))
buildEarley prods start = (\ ~e -> lookup' start e) <$> (mfix interpEarley)
  where
    -- mkEnv e = Map.foldlWithKey (\acc k v -> do vs <- acc; v' <- v; pure $ Map.insert k v' vs) (pure $ Map.empty) (Map.map rule e)
    -- mkEnv e = error "todo"
    interpEarley env = foldM monadWorker env (map (\(lhs, alts) -> interpProds env lhs alts) $ Map.toList prods)

    -- monadWorker = error "todo"
    monadWorker :: LazyMap String (Prod r e Char String) -> (String, Grammar r (Prod r e Char String)) -> Grammar r (LazyMap String (Prod r e Char String))
    monadWorker env (nme, rle) = do ret <- rle; pure $ assoc nme ret env
    -- interpEarley = error "TODO"

recognize :: String -> Gen.Grammar -> String -> Bool
recognize s gram start = let rep = report $ parser (buildEarley gram start) in 
  (LL.null . unconsumed) $ rep s
-- recognize = error "TODO"

  