{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Text.PrettyTemplate
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Char
import qualified Data.Text.IO as T
import Control.Exception
import qualified Text.PrettyPrint.ANSI.Leijen as C

data ClassDef = ClassDef { 
      imports :: [String],
      name :: String,
      parent :: Maybe String,
      members :: [MemberDef]
    }

data Visibility = Private | Public 

data MemberDef
    = Field { 
        mVisibility ::  Visibility, 
        mTy :: String,
        mName :: String
        }
    | Method {
        mVisibility :: Visibility,
        mName :: String,
        mResult :: String,
        mArgs :: [ArgDef],
        mBody :: String
        }

data ArgDef = ArgDef { aTy :: String, aName :: String }

deriveJSON defaultOptions{fieldLabelModifier = map Data.Char.toLower . drop 1, 
                          constructorTagModifier = map  Data.Char.toLower
                         } ''ArgDef
deriveJSON defaultOptions{constructorTagModifier = map Data.Char.toLower
                         } ''Visibility
deriveJSON defaultOptions{fieldLabelModifier = map Data.Char.toLower . drop 1,
                          constructorTagModifier = map Data.Char.toLower
                         } ''MemberDef
deriveJSON defaultOptions{constructorTagModifier = map Data.Char.toLower
                         } ''ClassDef

test1 :: ClassDef
test1 = ClassDef {
          imports = ["java.com","java.class"],
          name = "TestClass",
          parent = Just "TestParent",
          members = [
           Field {
             mVisibility = Private,
             mTy = "int",
             mName = "myvar"
           },
           Method {
             mVisibility = Public,
             mResult = "int",
             mName = "getMyVar",
             mArgs = [],
             mBody = "return myvar;"
           },
           Method {
             mVisibility = Public,
             mResult = "void",
             mName = "setMyVar",
             mArgs = [ArgDef "int" "v"],
             mBody = "myvar = v;"
           }
          ]
        }

{-
test1 :: Val
test1 = obj [
         "imports"  =: Arr [
                        c "java.com", c "java.class"
                       ],
         "name"    =: c "TestClasS",
         "parent"  =: c "ParrenatS",
         "members" =: Arr [
                       "field" :@ obj [
                                    "visibility"  =: c "private",
                                    "type"        =: c "int",
                                    "name"        =: c "myvara\n- 2"
                                   ],
                       "method" :@ obj [
                                     "visibility" =: c "public",
                                     "type"       =: c "int",
                                     "name"       =: c "getMyvara",
                                     "args"       =: Arr [],
                                     -- TODO: body as Doc
                                     "body"       =: c "return myvara;" 
                                    ],
                       "method" :@ obj [
                                     "visibility" =: c "public",
                                     "type"       =: c "void",
                                     "name"       =: c "setMyvara",
                                     "args"       =: Arr [
                                                      obj [
                                                       "type" =: c "int", 
                                                       "name" =: c "v"
                                                      ]
                                                     ],
                                     "body"       =: c "myvara = v;"
                                    ]
                      ]
        ]

-}

main :: IO ()
main = outputTemplateError (T.putStrLn =<< applyFileTemplate "test1.pt" test1)

