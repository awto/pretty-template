{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.PrettyTemplate
import qualified Data.Map as M

obj = Obj . M.fromList
l =: r = (l, r)
c = Const

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

main :: IO ()
main = putStrLn =<< applyFileTemplate "test1.pt" test1
