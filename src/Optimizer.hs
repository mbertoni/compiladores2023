module Optimizer (opt, hasEffects, deadCodeElimination, isUsed, addReferences) where

import Core 
import Subst
import MonadFD4
import Global

opt :: TTerm -> TTerm
opt = go fuel where
    fuel = 10
    go:: Int -> TTerm -> TTerm 
    go 0 tt = tt
    go n tt = t3 
        where   t1 = constantFolding tt
                t2 = constantPropagation t1
                t3 = constReplacing t2
                t4 = go (n-1) t3

constantFolding :: TTerm -> TTerm
constantFolding = visit go
    where   go (IfZ i c t e) = case c' of 
                Lit _ (N 0) -> go t
                Lit _ (N _) -> go e
                term -> IfZ i c' t' e
                where
                    c' = constantFolding c -- no estoy seguro si es visit go o solamente go
                    t' = constantFolding t
                    e' = constantFolding e
            go (BOp i op t1 t2) =  case t2' of 
                -- Tendríamos que ver el print acá, ¿no?
                    -- Si t2 es 0, retorno t1
                    Lit _ (N 0) -> t1' 
                    _ -> case t1' of    Lit _ (N 0) -> 
                                            case op of  Add -> t2'
                                                        Sub -> Lit i (N 0)
                                        _ -> BOp i op t1' t2' 
                    where   t1' = constantFolding t1
                            t2' = constantFolding t2
            go term = visit go term    

constantPropagation :: TTerm -> TTerm
constantPropagation = visit go
    where   go (Let i x xty alias (Sc1 body)) = case alias' of
                Lit i2 l -> Let i x xty alias' (Sc1 substBody)
                                where   body' = subst (Lit i2 l) (Sc1 body)
                                        substBody = visit go body'
                term     -> Let i x xty alias' (Sc1 substBody)
                                where substBody = visit go body
                where alias' = visit go alias
            go term = visit go term


constReplacing :: TTerm -> TTerm
constReplacing = visit go
    where
        go :: TTerm -> TTerm
        go (App i (Lam _ _ _ scope) l@(Lit _ _)) = constReplacing $ subst l scope
        go (App i (Lam _ nm ty scope) t) = constReplacing $ (Let i nm ty (go t) scope)
        go t = t

inLine :: TTerm -> TTerm
inLine = visit go
    where
        go :: TTerm -> TTerm
        go t@(Let i x xty alias scope@(Sc1 body)) = if isSimple then go $ subst alias scope else t
            where isSimple = False -- ver cómo calculamos esto
        go t = t

-- Deberíamos tenerlo en cuenta para contant folding y para subexp elimination <si la implementamos>
isPure :: TTerm -> Bool
isPure (Lit _ _) = True
isPure (Pnt _ _ _) = False
isPure (Var _ (Free _)) = True
isPure (Var _ (Bound _)) = True
isPure (Var _ (Global _)) = False
isPure (Lam _ _ _ (Sc1 t)) = isPure t
isPure (App _ f x) = isPure f && isPure x
isPure (Fix _ _ _ _ _ (Sc2 t)) = isPure t
isPure (BOp _ _ x y) = isPure x && isPure y
isPure (IfZ _ c t e) = isPure c && isPure t && isPure e
isPure (Let _ _ _ alias (Sc1 bdy)) = isPure alias && isPure bdy

hasEffects :: TTerm -> Bool
hasEffects (Var _ _) = False
hasEffects (Lit _ _) = False
hasEffects (Lam _ n _ bdy) = hasEffects (open n bdy)
hasEffects (App _ f x) = hasEffects f || hasEffects x
hasEffects (Pnt _ _ _ ) = True
hasEffects (BOp _ o x y) = hasEffects x || hasEffects y
hasEffects (Fix _ f _ x _ bdy) = hasEffects (open2 f x bdy)
hasEffects (IfZ _ c t f) = hasEffects c || hasEffects t || hasEffects f
hasEffects (Let _ x xty alias bdy) = hasEffects alias || hasEffects (open x bdy)

addReferences :: MonadFD4 m => TTerm -> m [Decl TTerm]
addReferences t@(Var (i,ty) (Global n)) = do 
  addReferencedVariable n
  return [] -- Ver qué completar acá
addReferences (Lam _ _ _ (Sc1 t)) = addReferences t
addReferences (App _ f x) = do
  r1 <- addReferences f
  r2 <- addReferences x
  return $ r1 ++ r2
addReferences (Pnt _ _ t) = addReferences t
addReferences (BOp _ _ t1 t2) = do
  r1 <- addReferences t1
  r2 <- addReferences t2
  return $ r1 ++ r2
addReferences (Fix _ _ _ _ _ (Sc2 t)) = addReferences t
addReferences (IfZ _ c t f) = do
  r1 <- addReferences c
  r2 <- addReferences t
  r3 <- addReferences f
  return $ r1 ++ r2 ++ r3
addReferences (Let _ _ _ alias (Sc1 bdy)) = do
  r1 <- addReferences alias
  r2 <- addReferences bdy
  return $ r1 ++ r2
{-  Lit   _       _
    Var _ (Bound _)
    Var _ (Free  _)
-}
addReferences _ = return []

deadCodeElimination :: MonadFD4 m => [Decl TTerm] -> m [Decl TTerm]
deadCodeElimination [] = return []
deadCodeElimination ds = do 
  s <- get
  let noDeadDecls = filter (\d -> ( not (mustBeFiltered d.body (s getUsedVariables)) )) ds 
  return noDeadDecls
    

isGlobalVar :: TTerm -> Bool
isGlobalVar (Var (i,ty) (Global n)) = True
isGlobalVar _ = False

-- Devuelve True cuando es una variable global, no usada y sin efectos.
-- El código con efectos no podemos fletarlo, incluso aunque no se use.
mustBeFiltered :: TTerm -> [Name] -> Bool
mustBeFiltered t referredVariables = isGlobalVar t && not (isUsed t referredVariables) && not (hasEffects t)

isUsed :: TTerm -> [Name] -> Bool
isUsed (Var _ (Global n)) referredVariables = elem n referredVariables
isUsed _ _ = False