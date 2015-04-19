module Text.PrettyTemplate(Template(..),applyFileTemplate,applyTemplate,
                          TemplateError(..),outputTemplateError) where

import Data.Aeson
import qualified Data.Text as T
import Data.Text(Text)
import Control.Arrow(right)
import Control.Monad.Error
import Control.Exception

import Text.PrettyTemplate.Syntax
import Text.PrettyTemplate.Parser
import Text.PrettyTemplate.Printer

applyTemplate :: ToJSON a => Template -> a -> IO Text
applyTemplate t v = applyRender t (toJSON v)

applyFileTemplate :: ToJSON a => String -> a -> IO Text
applyFileTemplate n d = flip applyTemplate d =<< parseFile n

