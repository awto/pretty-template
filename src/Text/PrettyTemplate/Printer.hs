{-# LANGUAGE PackageImports #-}
module Text.PrettyTemplate.Printer where

import Text.PrettyTemplate.Syntax
import Control.Monad.Reader
import Control.Monad.Error
import "wl-pprint-extras" Text.PrettyPrint.Free
import Data.Char
import Data.Int
import qualified Data.Map as M
import Text.Trifecta.Delta

data Env = Env {
      stack_  :: [Val],
      mcol_   :: !Int64,
      odelta_ :: !Int64,
      ocol_   :: !Int
    }

type D = ErrorT String (Reader Env)

err :: String -> D a
err = throwError

(<?>) :: D a -> String -> D a
a <?> t = a `catchError` \e -> throwError $ e ++ " " ++ t
infix 3 <?>

apply :: Val -> Template -> Either String (Doc e)
apply v t = runReader (runErrorT (docTemplate t)) (Env [v] 0 0 0)

docArr :: Arr -> D (Doc e)
docArr (A p a opts) = return . applyOpts opts =<< case a of
     Var v t e -> (maybe docE docT =<< getVar v)
                      <?> "for variable " ++ show v ++ " at " ++ show (pretty p)
        where
        docC vv = case vv of
                  Const vt      -> return . vcat . map text . lines $ vt
                  ConstD (TD d) -> return d
                  _             -> err "not a ground variable"
        docT vv = maybe (docC vv) (newv vv) t 
        docE = maybe (err "couldn't find variable") (docArr) e
     Template t -> local (\x -> x{ocol_ = fromEnum (Text.Trifecta.Delta.column p) + 2}) 
                   $ docTemplate t
     List lopts t s e -> do
       vv <- headVal
       case vv of
         Arr [] -> maybe (return empty) docArr e
         Arr av -> do
                 e' <- mapM (flip newv t) av
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
                    LVCat    -> hcat e''
                    LFillCat -> fillCat e''
                    LCat     -> cat e''
         _      -> err "not an array's value"
     Case d -> do
       h <- headVal
       case h of
         n :@ hv -> maybe (err $ "no such case alternative: " ++ show n) (newv hv)
              $ n `M.lookup` d
         _           -> err "case for not tagged value"
     Mod m -> return $ case m of
                         SoftLine  -> softline
                         Line   -> line
                         SBreak -> softbreak 
                         LBreak -> linebreak
     _ -> err "not implemented"
  where
  newv vv = local (\x -> x{stack_ = vv : stack_ x}) . docArr

docTxt :: Delta -> String -> Delta -> D (Doc e)
docTxt _ t _ =do
  c <- asks ocol_
  return $ case lines t of
             [t'] -> text t'
             t' -> hlp c t'
      where
        hlp c = vcat . map (text . l c)
        l c n = sp' ++ ns
            where
              (sp,ns) = span Data.Char.isSpace n
              sp' = drop (fromEnum c) sp

applyOpts :: [TOpt] -> Doc e -> Doc e
applyOpts [] d = align d
applyOpts opts d = foldr (\o d' -> case o of
            TGroup -> group d'
            THang -> hang 4 d'
            TNest -> nest 4 d'
            TAlign -> align d'
            TDef -> d'
            _ -> align d') d opts

docTemplate' :: TemplateItem Arr -> D (Doc e)
docTemplate' (Left (Txt i t o))  = docTxt i t o
docTemplate' (Right a)           = return . applyOpts (arrOpts a) =<< docArr a

docTemplate :: Template -> D (Doc e)
docTemplate = return . hcat <=< mapM docTemplate'

getVar' :: [Val] -> Var -> D (Maybe Val)
getVar' (Obj m : _) (N n)    = return $ n `M.lookup` m 
getVar' (_ : t)     (Back v) = getVar' t v
getVar' (h : _)     Self     = return (Just h)
getVar' (h : _)     n        = err $ "cannot reference variable: " 
                                    ++ show n ++ " in " ++ show h
getVar' []          n        = err $ "empty stack for " ++ show n

headVal :: D Val
headVal = do
  v <- asks stack_
  case v of
    (h : _) -> return h
    _       -> err "empty stack"

getVar :: Var -> D (Maybe Val)
getVar n = do
  d <- asks stack_
  getVar' d n
