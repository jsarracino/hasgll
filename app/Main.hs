module Main where

import Parser ( program, sentence )
import Grammar ( pp, Grammar )
-- import Interpreter (parses)
import FancyInterp (parses)
import Backend.Earley
import Backend.PEG
import Logic.OrderPEG
import Logic ( fuzzAllProds, compileOneProd )

import qualified Output.PEG as PEGO
import Output.CFG as CFGO

import qualified Data.Map.Strict as Map
-- import qualified Data.Set as S

import Z3.Monad ( evalZ3, Z3) 

import Logic.Schedule 

import Control.Monad
import System.Environment

expr = program "Expr ::= 1 | Expr + Expr | Expr * Expr"

expr_left = program "Expr ::= 1 | 1 + Expr | 1 * Expr "
expr_left_one = program "Expr ::= 1 + Expr | 1 | 1 * Expr "
expr_left_peg = program "Expr ::= 1 + Expr |  1 * Expr | 1 "

conditional_var = program "Base ::= 1 ; Expr ::= Base | if Expr then Expr | if Expr then Expr else Expr"


expr_parens = program "Expr ::= 1 | Expr + Expr | Expr * Expr | \\( Expr \\)"
expr_factored = program "Expr ::= Expr + Term | Term ; Term ::= Term * Factor | Factor ; Factor ::= 1 | \\( Expr \\)"

order_left :: PickleOrd
order_left = Map.fromList [(sentence "1", Map.fromList [(sentence "1 + Expr", Just GT), (sentence "1 * Expr", Just GT)])]

-- main :: IO ()
-- main = do 
--   putStrLn "input grammar:"
--   gram <- getLine
--   putStrLn $ "grammar is " ++ (pp . program) gram
--   putStrLn "input starting production:"
--   start <- getLine
--   putStrLn "input string to parse (control-C to quit)"
--   loopParse start (program gram)

gramToPeg :: Grammar -> Z3 (Either Grammar String)
gramToPeg gram = do 
  sched <- scheduleGram gram
  case sched of 
    Just lhss -> foldM (worker lhss) (Left gram) lhss
    Nothing -> pure $ Right "Couldn't make a dataflow ordering for the grammar"

  where
    spec = fuzzAllProds gram

    worker :: [String] -> Either Grammar String -> String -> Z3 (Either Grammar String)
    worker sched (Left g) s = do 
      g' <- compileOneProd g s spec [[]]
      case g' of 
        Just g'' -> pure $ Left g''
        Nothing -> pure $ Right $ "Failed compiling production: " ++ s ++ " with sched: " ++ show sched
    worker _ x@Right{} _ = pure x


loopParse :: String -> Grammar -> IO ()
loopParse start gram = do 
  str <- getLine
  putStrLn $ "parsed to " ++ (show $ map fst $ parses start gram str)
  loopParse start gram

fromFile :: FilePath -> IO Grammar
fromFile pth = program <$> readFile pth 

readWritePEG :: String -> String -> IO ()
readWritePEG inp out = do 
  gram <- program <$> readFile inp
  putStrLn $ "Loaded CFG: " ++ pp gram
  pegOrError <- evalZ3 (gramToPeg gram)
  case pegOrError of 
    Left g' -> let pego = PEGO.pp g' in do putStrLn ("Generated PEG: " ++ pego) ; writeFile out pego
    Right msg -> putStrLn $ "encountered error: " ++ msg

main :: IO ()
main = do 
  args <- getArgs
  case args of 
    inputName : outputName : _ -> readWritePEG inputName outputName
    _ -> putStrLn $ "error, missing input and output file name"