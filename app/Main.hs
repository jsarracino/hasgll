module Main where

import Parser

main :: IO ()
main = do 
  lne <- getLine
  print $ (parse . lexer) lne
