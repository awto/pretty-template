{-# LANGUAGE PackageImports,RankNTypes,TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell,FlexibleInstances,DeriveDataTypeable #-}
module Text.PrettyTemplate.Syntax where

import qualified Data.HashMap.Strict as M
import Text.Trifecta(Span(..),HasSpan(..))
import Text.Trifecta.Delta(Delta)
import Text.PrettyPrint.Leijen.Text as D(Doc)
import Text.PrettyPrint.ANSI.Leijen as C(Doc,displayIO,renderPretty)
import qualified Data.Text as T
import Data.Text(Text)
import Data.Monoid
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Control.Lens
import Control.Lens.TH
import Control.Exception
import Data.Typeable
import System.IO(stderr)

data Txt = Txt { _txtPos :: Span, _txtVal :: Text }
         deriving Show

spanStart :: Lens' Span Delta
spanStart f (Span s e t) = fmap (\s' -> Span s' e t) (f s) 
spanEnd :: Lens' Span Delta
spanEnd f (Span s e t) = fmap (\e' -> Span s e' t) (f e) 

type TemplateItem a = Either Txt a

type Template' a = [TemplateItem a]

type Template = Template' Arr

type Dict a = M.HashMap Text a

data Var = N !Text                -- ^ Variable's name
         | Back !Var              -- ^ Parent record of the field
         | Self                   -- ^ Current value
           deriving (Show, Eq)

data LOpts = LNone | LHSep | LVSep | LFillSep 
           | LSep | LHCat | LVCat | LFillCat | LCat
             deriving Show

data TOpt = TNone | TDef | TRaw | TGroup | THang | TAlign | TNest
            deriving Show

data Mod = SoftLine | Line | SBreak | LBreak
           deriving Show

data Arr' a = Var !Var !(Maybe a) !(Maybe a)       -- ^ Variable with possible 
            | Template Delta !(Template' a)       -- ^ Move to object level
            | List !LOpts !a !(Maybe a) !(Maybe a) -- ^ List 
            | Case !Text !(Dict a)                 -- ^ Tag based sub-template selection
            | Mod !Mod                             -- ^ Layout control modifiers
            | Up !a                                -- ^ Up level (not used now)
            | Down !a                              -- ^ Down level (not used now)
              deriving (Show)

data Arr = A { _arrPos :: !Span, 
               _arrIn :: !(Arr' Arr), 
               _arrOpts :: ![TOpt] }

posEmpty = Span mempty mempty BS.empty

mkArr = A posEmpty
mkTxt = Txt posEmpty

instance Show Arr where
    showsPrec x (A _ n _) = showsPrec x n

makeLenses ''Arr
makeLenses ''Txt

instance HasSpan Arr where span = arrPos
instance HasSpan Txt where span = txtPos

-- | common exception for all templates problems
newtype TemplateError = TemplateError C.Doc
    deriving (Show,Typeable)

instance Exception TemplateError

-- | simply redirects catched errors to stderr and ignores them
outputTemplateError :: IO () -> IO ()
outputTemplateError a = a `catch` 
                        \(TemplateError e) -> C.displayIO stderr 
                                              $ C.renderPretty 0.7 80 e


