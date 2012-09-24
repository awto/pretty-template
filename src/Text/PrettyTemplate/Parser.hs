{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.PrettyTemplate.Parser where

import Text.PrettyTemplate.Syntax

import Control.Applicative
import qualified Data.Map as M
import Text.Trifecta
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Control.Monad (MonadPlus(..))
import Text.Trifecta.Delta
import qualified Data.HashSet as HashSet
import Data.List
import Data.Semigroup

idstyle :: TokenParsing m => IdentifierStyle m
idstyle = emptyIdents{ styleReserved 
                        = HashSet.fromList ["$[","$]","${-","$-}","$--", "+{",
                                            "${","$}","/","%","*","?", "{", "}", 
                                            ",", "cat", "$/"
                                            ] }

lopts :: TokenParsing m => m LOpts
lopts = rw "/" *> (
                   LHSep <$ rw "hsep"
                   <|> LVSep <$ rw "vsep" 
                   <|> LFillSep <$ rw "fillsep"
                   <|> LSep <$ rw "lsep"
                   <|> LHCat <$ rw "hcat"
                   <|> LVCat <$ rw "vcat"
                   <|> LFillCat <$ rw "fillcat"
                   <|> LCat <$ rw "cat"
                   <|> pure LVSep
                   )
        <|> pure LHCat


topt :: TokenParsing m => m TOpt
topt = try $ rw "/" *> (
                   TRaw <$ rw "raw"
                   <|> TGroup <$ rw "group"
                   <|> THang <$ rw "hang"
                   <|> THang <$ rw ">>"
                   <|> TNest <$ rw ">"
                   <|> TNest <$ rw "nest"
                   <|> TAlign <$ rw "align"
                   <|> TAlign <$ rw "|"
                   <|> TDef <$ rw "-"
                   )

iden :: TokenParsing m => m String
iden = ident idstyle

var :: TokenParsing m => m Var
var = Self <$ rw "%"
      <|> Back <$ rw "^" <*> var
      <|> N <$> iden

arr' :: DeltaParsing m => m Arr
arr' = mkA (
            List <$ rw "*" <*> lopts <*> arr' <*> optional (rw "," *> arr')     
                <*> optional (rw "." *> arr')
            <|> Var <$> var <*> optional arr' <*> optional (rw "." *> arr')
            <|> Template <$ try (string "$[") <*> template' <* rw "$]"
            <|> Case <$ rw "{" <*> dictCont <* rw "}"
            <|> Up <$ rw "+" <*> arr'
            <|> Down <$ rw "-" <*> arr'
           ) 
       <|> parens arr 

dictCont :: DeltaParsing m => m (Dict Arr)
dictCont = M.fromList <$> commaSep ((,) <$> iden <* rw ":" <*> arr)

mkA :: DeltaParsing m => m (Arr' Arr) -> m Arr
mkA p = A <$> position <*> p <*> pure []

updOpts :: Arr -> [TOpt] -> Arr
updOpts (A p v o) o' = A p v (o ++ o') 

arr :: DeltaParsing m => m Arr
arr = updOpts <$> arr' <*> many topt
  
rw :: TokenParsing m => String -> m ()
rw n = () <$ token (try (highlight Operator (string n)))

template' :: DeltaParsing m => m Template
template' = many itm 
    where itm = Right <$ rw "${" <*> arr <* try (string "$}")
                <|> Right <$ rw "$/" <*> mkA (Mod <$> modp)
                <|> Left <$> dtxt

modp :: TokenParsing m => m Mod 
modp = SoftLine <$ rw "sline"
       <|> Line <$ rw "line"
       <|> SBreak <$ rw "sbreak"
       <|> LBreak <$ rw "lbreak"

template :: T Template
template = template'

eol :: CharParsing m => m ()
eol = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

tspace :: CharParsing m => Bool -> m ()
tspace eatSpaces = skipSome item
     where
       item = if eatSpaces then skipSome space <|> comment else comment
       comment = oneLineComment <|> multiLineComment <?> ""
       oneLineComment = try (string "$--") *> skipMany (noneOf "\n\r") <* eol
       multiLineComment = try (string "${-") *> inComment 
       inComment = () <$ try (string "$-}")
                   <|> multiLineComment *> inComment
                   <|> skipSome (noneOf "$") *> inComment
                   <|> oneOf "$" *> inComment

dtxt :: DeltaParsing m => m Txt
dtxt = Txt <$> position <*> (($"") <$> txt) <*> position

txt :: TokenParsing m => m ShowS
txt = cnt <$> some itm <?> "template text"
      where
        cnt = foldl' (.) id
        itm = id <$ tspace False
              <|> (
                   showChar '$' <$ try (string "$$")
                   <|> showString <$> some (noneOf "$") 
                   <|> (\n v -> showChar '$' . n . cnt v) 
                   <$> try (showChar <$ char '$' <*> noneOf "{]") 
                           <*> many itm
                  )

newtype T a = T { unT :: Parser a} 
    deriving (Functor, Applicative, Parsing, Monad, MonadPlus, Alternative, 
              CharParsing, DeltaParsing, MarkParsing Delta, Monoid, Semigroup)

instance TokenParsing T where
    someSpace = tspace True


parseFile :: String -> IO Template
parseFile arg = do
  r <- parseFromFile (unT template <* eof) arg
  case r of
    Nothing -> fail "parse error"
    Just t -> return t
       
