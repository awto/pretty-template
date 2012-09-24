module Text.PrettyTemplate(Val(..),TDoc,Template,applyFileTemplate,applyTemplate) where

import Text.PrettyTemplate.Syntax
import Text.PrettyTemplate.Parser
import Text.PrettyTemplate.Printer

import Control.Arrow(right)
import Control.Monad.Error

applyTemplate :: Template -> Val -> Either String String
applyTemplate t v = right show (apply v t)

applyFileTemplate :: String -> Val -> IO String
applyFileTemplate n d = either (throwError . strMsg) return . flip applyTemplate d 
                       =<< parseFile n
