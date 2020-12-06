{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}

module Backend.Grampa (
  -- recognize
  g1'
  , g2'
  , g3'
  , g4'
  , g5'
  , g6'
) where
import Control.Applicative
import Text.Grampa
import Text.Grampa.PEG.Packrat (Parser)
import Data.Functor.Classes (Show1, showsPrec1)
import qualified Rank2.TH
import Data.Char

data Arithmetic f = Arithmetic{sum     :: f String,
                               number  :: f String,
                               float   :: f String}

$(Rank2.TH.deriveAll ''Arithmetic)

g1 :: GrammarBuilder Arithmetic g Parser String
g1 Arithmetic{..} = Arithmetic{
   sum= (\l r -> l ++ "+" ++ r) <$> sum <* string "+" <*> sum <|> float <|> number,
   number= takeCharsWhile1 isDigit <?> "number",
   float= (\l r -> l ++ "." ++ r) <$> number <* string "." <*> number}

g2 :: GrammarBuilder Arithmetic g Parser String
g2 Arithmetic{..} = Arithmetic{
   sum= float <|> (\l r -> l ++ "+" ++ r) <$> sum <* string "+" <*> sum <|> number,
   number= takeCharsWhile1 isDigit <?> "number",
   float= (\l r -> l ++ "." ++ r) <$> number <* string "." <*> number}

g3 :: GrammarBuilder Arithmetic g Parser String
g3 Arithmetic{..} = Arithmetic{
   sum= float <|> number <|> (\l r -> l ++ "+" ++ r) <$> sum <* string "+" <*> sum,
   number= takeCharsWhile1 isDigit <?> "number",
   float= (\l r -> l ++ "." ++ r) <$> number <* string "." <*> number}

g4 :: GrammarBuilder Arithmetic g Parser String
g4 Arithmetic{..} = Arithmetic{
   sum= (\l r -> l ++ "+" ++ r) <$> sum <* string "+" <*> sum <|> number <|> float,
   number= takeCharsWhile1 isDigit <?> "number",
   float= (\l r -> l ++ "." ++ r) <$> number <* string "." <*> number}

g5 :: GrammarBuilder Arithmetic g Parser String
g5 Arithmetic{..} = Arithmetic{
   sum= number <|> (\l r -> l ++ "+" ++ r) <$> sum <* string "+" <*> sum <|> float,
   number= takeCharsWhile1 isDigit <?> "number",
   float= (\l r -> l ++ "." ++ r) <$> number <* string "." <*> number}

g6 :: GrammarBuilder Arithmetic g Parser String
g6 Arithmetic{..} = Arithmetic{
   sum= number <|> float <|> (\l r -> l ++ "+" ++ r) <$> sum <* string "+" <*> sum,
   number= takeCharsWhile1 isDigit <?> "number",
   float= (\l r -> l ++ "." ++ r) <$> number <* string "." <*> number}

g1' = fixGrammar g1
g2' = fixGrammar g2
g3' = fixGrammar g3
g4' = fixGrammar g4
g5' = fixGrammar g5
g6' = fixGrammar g6

instance Show1 f => Show (Arithmetic f) where
   show Arithmetic{..} =
      "Arithmetic{\n  sum=" ++ showsPrec1 0 sum
           (",\n  number=" ++ showsPrec1 0 number
           (",\n  float=" ++ showsPrec1 0 float "}"))

-- recognize g s = case Backend.Grampa.sum (parseComplete g s) of 
--                     Right{} -> True
--                     Left{} -> False