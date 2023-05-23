# hasgll
A semantics-preserving translater from GLL grammars to PEG grammers. PEG grammars are efficient but have unintuitive semantics due to their *ordered choice* operator, which restricts parser backtracking. 
This tool treats a GLL grammar as a *ground truth* for a PEG grammar. 
The key idea for translation is to replace GLL alternation with PEG ordered choice and reorder PEG terms
when there are *conflicts* (i.e. a subset relation) between the underlying PEG grammars. 

To check for grammar subsets, we "fuzz" by enumerating strings and checking for membership (see src/Logic.hs:subset).

Then, to build a conflict-free PEG grammar, we *linearize* the alternation by building a set of *ordering constraints* 
and using an SMT solver to convert the constraints into a total order (if it exists): see src/Logic.hs:compileOneProd.

## Dependencies
`hasgll` depends on `stack`, `happy`, and `Z3`, For `happy`, it needs an invocation of `stack install happy` to add
`happy` to the local stack installation.

For `Z3`, please add Z3 version 4.8.7 to your path.

## Building and Running

To build: `stack build`.

To run: `stack run -- <input-filename> <output-filename>`. There are several test files in test, which end in .gll.
The current passing tests are `calc-medium.gll` and `calc-large.gll`. 
`calc.gll`, `cycle.gll`, `cond-small.gll`, `antlr-DGSParser.gll` currently do not work.

For example:

```
john@MacBook-Pro-227 hasgll % stack run -- test/calc-large.gll output.peg
Loaded CFG: Base ::= Digit | Var | Bool ;
Bool ::= t r u e | f a l s e ;
Digit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 ;
Expr ::= Base |
         Base + Expr |
         Base * Expr |
         Base - Expr |
         Base / Expr |
         ( Expr ) |
         Base + + Expr |
         Base ^ Expr |
         Base & & Expr |
         Base > > Expr |
         Base < < Expr |
         i f Expr t h e n Expr e l s e Expr |
         Base > Expr |
         Base < Expr |
         Base = Expr |
         Base ! = Expr |
         Base < = Expr |
         Base > = Expr |
         Base | | Expr ;
Num ::= Digit | Digit Num ;
Var ::= x | y | z
Generated PEG: Base <- Digit /  Var /  Bool ;
Bool <- t r u e /  f a l s e ;
Digit <- 1 /  0 /  7 /  8 /  9 /  4 /  5 /  6 /  2 /  3 ;
Expr <- Base + Expr / 
        Base < < Expr / 
        i f Expr t h e n Expr e l s e Expr / 
        Base > Expr / 
        Base < Expr / 
        Base = Expr / 
        Base ! = Expr / 
        Base < = Expr / 
        Base > = Expr / 
        Base | | Expr / 
        Base * Expr / 
        Base - Expr / 
        Base / Expr / 
        ( Expr ) / 
        Base + + Expr / 
        Base ^ Expr / 
        Base & & Expr / 
        Base > > Expr / 
        Base ;
Num <- Digit Num /  Digit ;
Var <- x /  y /  z

john@MacBook-Pro-227 hasgll % cat output.peg
Base <- Digit /  Var /  Bool ;
Bool <- t r u e /  f a l s e ;
Digit <- 1 /  0 /  7 /  8 /  9 /  4 /  5 /  6 /  2 /  3 ;
Expr <- Base + Expr / 
        Base < < Expr / 
        i f Expr t h e n Expr e l s e Expr / 
        Base > Expr / 
        Base < Expr / 
        Base = Expr / 
        Base ! = Expr / 
        Base < = Expr / 
        Base > = Expr / 
        Base | | Expr / 
        Base * Expr / 
        Base - Expr / 
        Base / Expr / 
        ( Expr ) / 
        Base + + Expr / 
        Base ^ Expr / 
        Base & & Expr / 
        Base > > Expr / 
        Base ;
Num <- Digit Num /  Digit ;
Var <- x /  y /  z

```
