module Main where

import Parser ( program )
import Grammar ( pp, Grammar )
-- import Interpreter (parses)
import FancyInterp (parses)

-- import qualified Data.Set as S

expr = program "E ::= E + E | E * E | 1"

main :: IO ()
main = do 
  putStrLn "input grammar:"
  gram <- getLine
  putStrLn $ "grammar is " ++ (pp . program) gram
  putStrLn "input starting production:"
  start <- getLine
  putStrLn "input string to parse (control-C to quit)"
  loopParse start (program gram)

loopParse :: String -> Grammar -> IO ()
loopParse start gram = do 
  str <- getLine
  putStrLn $ "parsed to " ++ (show $ map fst $ parses start gram str)
  loopParse start gram
