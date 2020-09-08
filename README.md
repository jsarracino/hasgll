# hasgll
An interpreter for GLL grammars in Haskell. 

## Dependencies
`hasgll` depends on `stack`. It also depends on `happy`, and so needs an invocation of `stack install happy` to add
`happy` to the local stack installation.

## Building and Running

To build: `stack build`
To run: `stack run hasgll-exe`

## Interpreter input

The interpreter takes BNF grammars with semicolon separated definitions as input,
as well as a starting production. The empty string is denoted by '\e' and alternation by '|'. 

Next it loops on input strings
and parses them against the grammar and starting production.
For example:

```
john@MacBook-Pro-227 hasgll % stack run hasgll-exe                            
input grammar:
A ::= A a | \e | B ; B ::= b
grammar is A ::= A a | \e | B ; B ::= b
input starting production:
A
input string to parse (control-C to quit)
aaaa
parsed to [([Atm (Chr 'a'),Atm (Chr 'a'),Atm (Chr 'a'),Atm (Chr 'a')],([],fromList []))]
aaa
parsed to [([Atm (Chr 'a'),Atm (Chr 'a'),Atm (Chr 'a')],([],fromList []))]
aa
parsed to [([Atm (Chr 'a'),Atm (Chr 'a')],([],fromList []))]
a
parsed to [([Atm (Chr 'a')],([],fromList []))]
b
parsed to [([Atm (Chr 'b')],([],fromList []))]
ab
parsed to []
ba
parsed to [([Atm (Chr 'b'),Atm (Chr 'a')],([],fromList []))]
bbbbaaaaa
parsed to []
bbbaaa
parsed to []
bbaa
parsed to []
ba
parsed to [([Atm (Chr 'b'),Atm (Chr 'a')],([],fromList []))]
baaaaa
parsed to [([Atm (Chr 'b'),Atm (Chr 'a'),Atm (Chr 'a'),Atm (Chr 'a'),Atm (Chr 'a'),Atm (Chr 'a')],([],fromList []))]

parsed to [([],([],fromList [])),([Atm Eps],([],fromList []))]
```
