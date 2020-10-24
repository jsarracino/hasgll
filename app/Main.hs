module Main where

import Parser ( program )
import Grammar ( pp, Grammar )
-- import Interpreter (parses)
import FancyInterp (parses)
import Backend.Earley
import Backend.PEG

-- import qualified Data.Set as S

expr = program "Expr ::= 1 | Expr + Expr | Expr * Expr"
expr_left = program "Expr ::= 1 | 1 + Expr | 1 * Expr "
expr_left_peg = program "Expr ::= 1 + Expr | 1 |  1 * Expr "
expr_parens = program "Expr ::= 1 | Expr + Expr | Expr * Expr | \\( Expr \\)"
expr_factored = program "Expr ::= Expr + Term | Term ; Term ::= Term * Factor | Factor ; Factor ::= 1 | \\( Expr \\)"

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
main = do 
  putStrLn "input grammar file:"
  gramf <- getLine
  gram <- fromFile gramf
  -- putStrLn $ "grammar is " ++ (pp . program) gram
  putStrLn "input starting production:"
  start <- getLine
  putStrLn "input filepath to parse (control-C to quit)"
  loopParseFile start gram

loopParseFile :: String -> Grammar -> IO ()
loopParseFile start gram = do 
  str <- getLine
  parseForest <- (parses start gram) <$> readFile str
  putStrLn $ show $ length parseForest
  loopParseFile start gram