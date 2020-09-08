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

fail1       = "a"
success1    = "aa"
success2    = "aaa"
fail2       = "aaaaa"

foo :: ParseResult Char
foo = parse grammar1 fail1

bar = parse grammar1