# hasgll
An interpreter for GLL grammars in Haskell. 

## Dependencies
`hasgll` depends on `stack`, `happy`, and `Z3`, For `happy`, it needs an invocation of `stack install happy` to add
`happy` to the local stack installation.

For `Z3`, please add Z3 version 4.8.7 to your path.

## Building and Running

To build: `stack build`
To run: `stack run hasgll-exe`
To demo: `stack ghci`
and then the following commands:

```
*Main Backend Backend.ANTLR Backend.Earley Backend.GLL Backend.Grampa Backend.PEG FancyInterp Grammar Interpreter Lib Logic Logic.OrderPEG Output.CFG Output.PEG Parser> :module Main
Prelude Main> :module + Grammar Output.CFG Logic.OrderPEG Z3.Monad Logic
Prelude Main Grammar Output.CFG Logic.OrderPEG Z3.Monad Map Logic> pp expr_left
"Expr ::= 1 | 1 + Expr | 1 * Expr"
Prelude Main Grammar Output.CFG Logic.OrderPEG Z3.Monad Logic> spec = fuzzAllProds expr_left
Prelude Main Grammar Output.CFG Logic.OrderPEG Z3.Monad Logic> spec
fromList [([Chr '1'],["1"]),([Chr '1',Chr '*',Var "Expr"],["1*1","1*1+1","1*1+1+1","1*1+1+1+1","1*1+1+1+1+1","1*1+1+1*1","1*1+1*1","1*1+1*1+1","1*1*1","1*1*1+1","1*1*1+1+1","1*1*1*1"]),([Chr '1',Chr '+',Var "Expr"],["1+1","1+1+1","1+1+1+1","1+1+1*1","1+1+1*1*1","1+1*1","1+1*1+1","1+1*1+1*1","1+1*1*1","1+1*1*1+1","1+1*1*1*1","1+1*1*1*1*1"])]
Prelude Main Grammar Output.CFG Logic.OrderPEG Z3.Monad Logic> evalZ3 $ compileOneProd expr_left "Expr" spec [[]]
Just (fromList [("Expr",[[Chr '1',Chr '+',Var "Expr"],[Chr '1',Chr '*',Var "Expr"],[Chr '1']])])
```

