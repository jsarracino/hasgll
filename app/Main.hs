module Main where

import Parser ( program )
import Grammar ( pp )

main :: IO ()
main = do 
  lne <- getLine
  print $ (pp . program) lne
