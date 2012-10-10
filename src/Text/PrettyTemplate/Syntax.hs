{-# LANGUAGE PackageImports,RankNTypes,TypeSynonymInstances,FlexibleInstances #-}
module Text.PrettyTemplate.Syntax where

import qualified Data.Map as M
import "trifecta" Text.Trifecta.Delta
import qualified "wl-pprint-extras" Text.PrettyPrint.Free as D(Doc)

data Val = Obj (M.Map String Val)    -- ^ Record
         | Arr [Val]                 -- ^ List
         | String :@ Val             -- ^ Tagged value
         | Const String              -- ^ String constant
         | ConstD TDoc               -- ^ Embedded document constant (not used yet)
           deriving Show

newtype TDoc = TD (forall e . D.Doc e)

instance Show TDoc where
    show (TD d) = show d

data Txt = Txt !Delta String !Delta
    -- deriving Show

instance Show Txt where
    show (Txt _ v _) = show v

type TemplateItem a = Either Txt a

type Template' a = [TemplateItem a]

type Template = Template' Arr

type Dict a = M.Map String a

data Var = N String              -- ^ Variable's name
         | Back Var              -- ^ Parent record of the field
         | Self                  -- ^ Current value
           deriving (Show, Eq)

data LOpts = LNone | LHSep | LVSep | LFillSep 
           | LSep | LHCat | LVCat | LFillCat | LCat
             deriving Show

data TOpt = TNone | TDef | TRaw | TGroup | THang | TAlign | TNest
            deriving Show

data Mod = SoftLine | Line | SBreak | LBreak
           deriving Show

data Arr' a = Var Var (Maybe a) (Maybe a)          -- ^ Variable with possible 
            | Template (Template' a)               -- ^ Move to object level
            | List LOpts a (Maybe a) (Maybe a)     -- ^ List 
            | Case (Dict a)                        -- ^ Tag based sub-template selection
            | Mod Mod                              -- ^ Layout control modifiers
            | Up a                                 -- ^ Up level (not used now)
            | Down a                               -- ^ Down level (not used now)
              deriving (Show)

data Arr = A Delta (Arr' Arr) [TOpt]
    --      deriving (Show)

arrOpts :: Arr -> [TOpt]
arrOpts (A _ _ opts) = opts

instance Show Arr where
    showsPrec x (A _ n _) = showsPrec x n

instance HasDelta Arr where
    delta (A d _ _) = d

instance HasDelta Txt where
    delta (Txt i _ _) = i

instance HasDelta Template where
    delta (Left v  : _) = delta v
    delta (Right v : _) = delta v
    delta _             = error "empty template"
