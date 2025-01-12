module Optimizer (optim, hasEffects, deadCodeElimination, isUsed, addReferences) where

import Core 
import Subst
import MonadFD4
import Global
import Debug.Trace

optim :: Decl TTerm -> Decl TTerm
optim = go fuel where
    fuel = 10
    go:: Int -> Decl TTerm -> Decl TTerm 
    go 0 t0 = t0
    go n t0 = t4 
        where   t1 = constantFolding t0
                t2 = constantPropagation t1 
                -- t3 = if t1 /= t2 then trace ("\nPrevio cp, t1: \n" ++ show t1 ++ "\n" ++ "\nPost   cp, t2: \n" ++ show t2 ++ "\n") inLine t2 else inLine t2
                t3 = inLine t2
                t4 = go (n-1) t3

constantFolding :: Decl TTerm -> Decl TTerm
constantFolding dt = Decl{pos = dt.pos, name = dt.name, body = visit go dt.body}
    where   go ifT@(IfZ i c t e) = case c of 
                Lit _ (N 0) -> t
                Lit _ (N _) -> e
                _           -> ifT
            go t@(BOp i op t1 t2) =  case t2 of 
                -- Tendríamos que ver el print acá, ¿no?
                    -- Si t2 es 0, retorno t1
                    Lit _ (N 0) -> -- trace ("\nCaso t2 = 0, t: \n" ++ show t ++ "\n")
                                    t1 
                    _           -> case t1 of
                                      -- Si el primero es 0, y es un Add, devuelvo el segundo
                                      -- Si el primero es 0, y es un Sub, devuelvo 0
                                      Lit _ (N 0) -> 
                                        
                                        case op of    Add -> -- trace ("\nCaso t1 = 0 y Add, t: \n" ++ show t ++ "\n") 
                                                            t2
                                                      Sub -> -- trace ("\nCaso t1 = 0 y Sub, t: \n" ++ show t ++ "\n") 
                                                            Lit i (N 0)
                                      -- Default, no hago nada.
                                      _           ->  t
            go term = term    

constantPropagation :: Decl TTerm -> Decl TTerm
constantPropagation dt = Decl{pos = dt.pos, name = dt.name, body = visit go dt.body}
    where 
      go t@(Let i x xty l@(Lit _ _) sc) = Let i x xty l (close x $ substNoError l sc) -- trace ("por sustituir l: \n" ++ show l ++ "\nEn sc: \n" ++ show sc ++ "\n") 
      go t = t
      -- Deberíamos también tener en cuenta que hay que hacer constansPropagation para las declaraciones globales onda 
      -- let x = 5


inLine :: Decl TTerm -> Decl TTerm
inLine dt = Decl{pos = dt.pos, name = dt.name, body = visit go dt.body} 
-- CHEQUEAR BIEN
    where
        go :: TTerm -> TTerm
        go t@(Let i x xty alias@(Lit _ _) sc@(Sc1 body)) =  Let i x xty alias (close x $ substNoError alias sc) 
        go (App i (Lam _ _  _  sc) l@(Lit _ _)) = substNoError l sc
        -- go (App i (Lam _ f fty sc) x ) = go $ Let i z fty e (subst x sc)
        --     where z = getFresh
        go t = t

-- Deberíamos tenerlo en cuenta para contant folding y para subexp elimination <si la implementamos>
-- Es exactamente lo opuesto a hasEffects, excepto porque isPure (Var _ (Global _)) = False , ¿por?
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

-- Da true si hay algún Print
hasEffects :: TTerm -> Bool
hasEffects (Lit _ _) = False
hasEffects (Pnt _ _ _ ) = True
hasEffects (Var _ _) = False
hasEffects (Lam _ n _ bdy) = hasEffects (open n bdy)
hasEffects (App _ f x) = hasEffects f || hasEffects x
hasEffects (Fix _ f _ x _ bdy) = hasEffects (open2 f x bdy)
hasEffects (BOp _ o x y) = hasEffects x || hasEffects y
hasEffects (IfZ _ c t f) = hasEffects c || hasEffects t || hasEffects f
hasEffects (Let _ x xty alias bdy) = hasEffects alias || hasEffects (open x bdy)

addReferences :: MonadFD4 m => TTerm -> m [TTerm]
addReferences t@(Var (i,ty) (Global n)) = do 
  -- addReferencedVariable n
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
{-  Lit   _    _
    Var _ (Bound _)
    Var _ (Free  _)
-}
addReferences _ = return []

deadCodeElimination :: (MonadFD4 m) => m [Decl TTerm]
deadCodeElimination = do 
  ds <- gets termEnvironment
  variables <- gets usedVariables
  let noDeadDecls = filter (\d -> ( not (mustBeFiltered d.body variables) )) ds 
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