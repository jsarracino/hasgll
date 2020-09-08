module Main where

import Parser ( program )
import Grammar ( pp )
import Interpreter

expr = program "E ::= E + E | E * E | 1"

main :: IO ()
main = do 
  lne <- getLine
  print $ (pp . program) lne
