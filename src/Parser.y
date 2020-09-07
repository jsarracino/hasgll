{
module Parser where

import qualified Grammar as G
import Data.Char
import qualified Data.Map.Strict as Map
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
  '::='             { TokenDef }
  var             { TokenVar $$ }
  chr             { TokenChar $$ }
  '|'             { TokenAlt }
  '('             { TokenLP }
  ')'             { TokenRP }
  '\e'            { TokenEps }
  ';'             { TokenSemi }

%left '|'

%%
Def : var '::=' Alts          { [($1, $3)] }
    | var '::=' Alts ';' Def  { ($1, $3) : $5 }

Word  : var { G.Var $1 }
      | chr { G.Chr $1 }
      | '\e' { G.Eps }

Sentence : Word { [$1] }
         | Word Sentence { $1 : $2 }

Alts : Sentence { [$1] }
     | Sentence '|' Alts { $1 : $3 }


{

-- The token type:
data Token =
	  TokenDef
  | TokenVar String
  | TokenChar Char
  | TokenAlt
  | TokenLP
  | TokenRP
  | TokenEps
  | TokenSemi
	deriving (Eq,Show,Ord)

lexer :: String -> [Token]
lexer [] = []
lexer (':':':':'=':cs) = TokenDef : lexer cs
lexer ('|':cs) = TokenAlt : lexer cs
lexer ('(':cs) = TokenLP : lexer cs
lexer (')':cs) = TokenRP : lexer cs
lexer (';':cs) = TokenSemi : lexer cs
lexer ('\\':'e':cs) = TokenEps : lexer cs
lexer (c:cs) 
  | isSpace c = lexer cs
  | isUpper c = lexVar (c:cs)
  | otherwise = TokenChar c : lexer cs

lexVar cs = case span isAlpha cs of
  (var,rest)   -> TokenVar var : lexer rest

parseError :: [Token] -> a
parseError _ = error "Parse error"

program :: String -> G.Grammar
program = Map.fromList . parse . lexer

}