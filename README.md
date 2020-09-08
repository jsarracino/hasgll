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
and parses them against the grammar and starting production. The empty string behavior is a bit buggy.
For example:

```
john@MacBook-Pro-227 hasgll % stack run hasgll-exe                            
input grammar:
A ::= a A | \e | B ; B ::= b
grammar is A ::= a A | \e | B ; B ::= b
input starting production:
A
input string to parse (control-C to quit)
aaaa
parsed to []
aaa
parsed to []
aa
parsed to []
a
parsed to []
b
parsed to [([Chr 'b'],fromList [("B",0)])]
ab
parsed to [([Chr 'a',Chr 'b'],fromList [("A",1),("B",1)])]
aab
parsed to [([Chr 'a',Chr 'a',Chr 'b'],fromList [("A",1),("A",2),("B",2)])]

parsed to [([Eps],fromList []),([Eps],fromList [])]
```
