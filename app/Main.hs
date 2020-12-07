module Main where

import Parser ( program, sentence )
import Grammar ( pp, Grammar )
-- import Interpreter (parses)
import FancyInterp (parses)
import Backend.Earley
import Backend.PEG
import Logic.OrderPEG
import Logic ( fuzzAllProds, compileOneProd )

-- import qualified Output.PEG as PEGO
import Output.CFG ()

import qualified Data.Map.Strict as Map
-- import qualified Data.Set as S

import Z3.Monad ( evalZ3 ) 

expr = program "Expr ::= 1 | Expr + Expr | Expr * Expr"

expr_left = program "Expr ::= 1 | 1 + Expr | 1 * Expr "
expr_left_one = program "Expr ::= 1 + Expr | 1 | 1 * Expr "
expr_left_peg = program "Expr ::= 1 + Expr |  1 * Expr | 1 "



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


loopParse :: String -> Grammar -> IO ()
loopParse start gram = do 
  str <- getLine
  putStrLn $ "parsed to " ++ (show $ map fst $ parses start gram str)
  loopParse start gram

fromFile :: FilePath -> IO Grammar
fromFile pth = program <$> readFile pth 

main :: IO ()
-- main = do 
--   putStrLn "input grammar file:"
--   gramf <- getLine
--   gram <- fromFile gramf
--   -- putStrLn $ "grammar is " ++ (pp . program) gram
--   putStrLn "input starting production:"
--   start <- getLine
--   putStrLn "input filepath to parse (control-C to quit)"
--   loopParseFile start gram

-- loopParseFile :: String -> Grammar -> IO ()
-- loopParseFile start gram = do 
--   str <- getLine
--   parseForest <- (parses start gram) <$> readFile str
--   putStrLn $ show $ length parseForest
--   loopParseFile start gram
main = do 
  putStrLn $ "input CFG: " ++ pp expr_left
  mg <- evalZ3 $ compileOneProd expr_left "Expr" spec [[]]
  case mg of 
    Just g -> putStrLn $ "compiled to: " ++ show g
    Nothing -> putStrLn "compilation failed :("
  where
    spec = fuzzAllProds expr_left