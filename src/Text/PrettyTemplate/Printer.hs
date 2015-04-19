{-# LANGUAGE TemplateHaskell #-}
module Text.PrettyTemplate.Printer where

import Text.PrettyTemplate.Syntax
import Control.Monad.Reader
import Text.PrettyPrint.Leijen.Text
import Data.Char
import Data.Int
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Traversable as F
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Text.Trifecta.Delta(Delta)
import qualified Text.Trifecta.Delta as P
import qualified Text.Trifecta as P
import qualified Control.Lens as L
import Control.Lens.TH
import Control.Lens.Operators
import qualified Text.PrettyPrint.ANSI.Leijen as C
import Control.Exception
import System.IO(stderr)
import Data.Aeson.Types
import Data.Aeson.Encode(encodeToTextBuilder)

data Env = Env {
      _stack  :: ![Value],
      _ocol :: !Int,
      _curPos :: !P.Span
    }

makeLenses ''Env

type D = ReaderT Env IO

posMsg :: C.Doc -> D C.Doc
posMsg d = do
  s <- L.view curPos
  return $ d C.<+> C.parens (C.pretty (s ^. spanStart)) 
         C.<$$>  C.pretty (P.render s) C.<> C.linebreak

errDoc :: C.Doc -> D C.Doc
errDoc e = do
  em <- posMsg e
  return $ C.red (C.text "ERROR:") C.<+> em 

err :: C.Doc -> D a
err = throw . TemplateError <=< errDoc

errS :: String -> D a
errS = err . C.text

warnDoc :: C.Doc -> D C.Doc
warnDoc e = do
  em <- posMsg e
  return $ C.yellow (C.text "WARNING:") C.<+> em 

warn :: C.Doc -> D ()
warn = liftIO . C.displayIO stderr . C.renderPretty 0.8 80 <=< warnDoc

warnS :: String -> D ()
warnS = warn . C.text

check :: String -> Maybe a -> D a
check e Nothing  = errS e
check _ (Just v) = return v

lu :: (Show a, Hashable a, Eq a) => String -> M.HashMap a b -> a -> D b
lu txt d k = case k `M.lookup` d of
               Just v -> return v
               Nothing -> err $ C.bold (C.text txt) 
                          C.<+> C.text (show k)

val :: FromJSON a => Value -> D a
val v = case fromJSON v of
          Error n -> err $ C.text "JSON parse error:" C.<+> C.text n
          Success v -> return v

apply :: Template -> Value -> IO Doc
apply t v = runReaderT (docTemplate t) (Env [v] 0 posEmpty)

applyRender :: Template -> Value -> IO Text
applyRender t v = apply t v >>= 
                  return . LT.toStrict . displayT . renderPretty 0.7 80 

docArr :: Arr -> D Doc
docArr (A p a o) = local (curPos .~ p) $ return . applyOpts o =<< case a of
     Var v t e -> docT =<< getVar v
        where
        docC vv = case vv of
                    String vt -> return . vcat . map text 
                                 . LT.lines . LT.fromStrict $ vt
                    Number s -> return $ text (LT.pack (show s))
                    _  -> do
                      warnS "not a string value (using plain JSON instead)"
                      return $ text $ LT.toLazyTextWith 1000 
                                 $ encodeToTextBuilder vv
        docT vv = maybe (docC vv) (newv vv) t
     Template p t -> local (ocol .~ fromEnum (P.column p)) $ docTemplate t
     List lopts t s e -> do
       vv <- headVal
       case vv of
         Array av | V.null av -> maybe (return empty) docArr e
         Array av -> do
                 e' <- mapM (flip newv t) (V.toList av)
                 e'' <- case s of
                      Nothing -> return e'
                      Just st -> do
                               st' <- docArr st
                               return $ punctuate st' e'
                 return $ case lopts of
                    LNone    -> hcat e''
                    LHSep    -> hsep e''
                    LVSep    -> vsep e''
                    LFillSep -> fillSep e''
                    LSep     -> sep e''
                    LHCat    -> hcat e''
                    LVCat    -> vcat e''
                    LFillCat -> fillCat e''
                    LCat     -> cat e''
         _      -> errS "not a list"
     Case f d -> do
       h <- headVal
       case h of
         Object obj -> docArr =<< lu "no such case option: " d
                    =<< val
                    =<< lu "no such object's field: " obj f
         _          -> errS "not an object in switch"
     Mod m -> return $ case m of
                         SoftLine  -> softline
                         Line   -> line
                         SBreak -> softbreak 
                         LBreak -> linebreak
     _ -> errS "not implemented"
  where
  newv vv = local (\x -> x{_stack = vv : _stack x}) . docArr

lines' :: Text -> [Text]
lines' ps | T.null ps   = []
          | otherwise = h : case T.uncons t of
                              Nothing -> []
                              Just (c,t')
                                  | c == '\n' -> T.lines t'
                                  | c == '\r' -> case T.uncons t' of
                                                   Just ('\n',t'') -> T.lines t''
                                                   _               -> T.lines t'
    where (h,t)    = T.span notEOL ps
          notEOL c = c /= '\n' && c /= '\r'

docTxt :: Text -> D Doc
docTxt t = do
  c <- asks _ocol 
  case lines' t of
    [t'] -> return $ text (LT.fromStrict t')
    t' -> do
      return $ hlp c t'
      where
        ltxt t | T.null t = text LT.empty -- linebreak
        ltxt t | otherwise = text (LT.fromStrict (l c t)) 
        hlp c = vcat . map ltxt
        l c n = T.concat [sp', ns]
            where
              (sp,ns) = T.span Data.Char.isSpace n
              sp' = T.drop (fromEnum c) sp

applyOpts :: [TOpt] -> Doc -> Doc
applyOpts [] d = align d
applyOpts opts d = foldr (\o d' -> case o of
            TGroup -> group d'
            THang -> hang 4 d'
            TNest -> nest 4 d'
            TAlign -> align d'
            TDef -> d'
            _ -> align d') d opts

docTemplate' :: TemplateItem Arr -> D Doc
docTemplate' (Left (Txt _ t))  = docTxt t
docTemplate' (Right a)         = return . applyOpts (a^.arrOpts) =<< docArr a

docTemplate :: Template -> D Doc
docTemplate = return . hcat <=< mapM docTemplate'

getVar' :: [Value] -> Var -> D Value
getVar' (Object m : _) (N n)    = return $ maybe Null id $  n `M.lookup` m 
getVar' (_ : t)        (Back v) = getVar' t v
getVar' (h : _)        Self     = return h
getVar' (h : _)        n        = err $ C.text "cannot reference variable " 
                                    C.<+> C.bold (C.text (show n))
                                    C.<+> C.text " in " 
                                    C.<+> C.bold (C.text (show h))
getVar' []             n        = err $ C.text "empty stack for " 
                                        C.<+> C.bold (C.text (show n))

headVal :: D Value
headVal = do
  v <- asks _stack
  case v of
    (h : _) -> return h
    _       -> errS "empty stack"

getVar :: Var -> D Value
getVar n = do
  d <- asks _stack
  getVar' d n
