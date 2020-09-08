module Reference (
    foo
  , bar
) where

import GLL.Parser
import GLL.Parseable.Char

-- instance Parseable Char where
--   eos = '$'
--   eps = '#'
grammar1 = (start "X" , [prod "X" [nterm "X", term 'a']
                      , prod "X" [term 'a']
                 ] )

success1       = "a"
fail1    = "aa"

foo :: ParseResult Char
foo = parse grammar1 fail1

bar :: [Char] -> ParseResult Char
bar = parse grammar1