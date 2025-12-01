module Optimizer (optim, hasEffects, deadCodeElimination, isUsed, addReferences) where

import Core
import Global
import MonadFD4
import Subst

optim :: Decl TTerm -> Decl TTerm
optim = go fuel
  where
    fuel = 10
    go :: Int -> Decl TTerm -> Decl TTerm
    go 0 t0 = t0
    go n t0 =
      let t1 = constantFolding t0
          t2 = constantPropagation t1
          t3 = inLine t2
          t4 = if t3 == t0 then t0 else go (n - 1) t3
       in t4

-- Constant Folding: calcula constantes y simplifica expresiones algebraicas
constantFolding :: Decl TTerm -> Decl TTerm
constantFolding dt = dt {body = go dt.body}
  where
    go :: TTerm -> TTerm

    -- Calcular constantes: 2 + 3 -> 5, 5 - 2 -> 3
    go (BOp i Add (Lit _ (N n1)) (Lit _ (N n2))) =
      Lit i (N (n1 + n2))
    go (BOp i Sub (Lit _ (N n1)) (Lit _ (N n2))) =
      Lit i (N (max 0 (n1 - n2))) -- evitar negativos

    -- Simplificaciones algebraicas: x + 0 -> x, 0 + x -> x
    -- Solo si no hay efectos
    go (BOp i Add t1 (Lit _ (N 0)))
      | not (hasEffects t1) = go t1
      | otherwise = BOp i Add (go t1) (Lit i (N 0))
    go (BOp i Add (Lit _ (N 0)) t2)
      | not (hasEffects t2) = go t2
      | otherwise = BOp i Add (Lit i (N 0)) (go t2)
    -- x - 0 -> x
    go (BOp i Sub t1 (Lit _ (N 0)))
      | not (hasEffects t1) = go t1
      | otherwise = BOp i Sub (go t1) (Lit i (N 0))
    -- Caso general para BOp
    go (BOp i op t1 t2) =
      let t1' = go t1
          t2' = go t2
          result = BOp i op t1' t2'
       in case result of
            -- Si después de procesar tenemos literales, calcular
            BOp _ Add (Lit _ (N n1)) (Lit _ (N n2)) -> Lit i (N (n1 + n2))
            BOp _ Sub (Lit _ (N n1)) (Lit _ (N n2)) -> Lit i (N (max 0 (n1 - n2)))
            BOp _ Add t1'' (Lit _ (N 0)) | not (hasEffects t1'') -> t1''
            BOp _ Add (Lit _ (N 0)) t2'' | not (hasEffects t2'') -> t2''
            BOp _ Sub t1'' (Lit _ (N 0)) | not (hasEffects t1'') -> t1''
            _ -> result
    go (IfZ i (Lit _ (N 0)) t e) = go t
    go (IfZ i (Lit _ (N _)) t e) = go e
    go (IfZ i c t e) =
      let c' = go c
          t' = go t
          e' = go e
       in case c' of
            Lit _ (N 0) -> t'
            Lit _ (N _) -> e'
            _ -> IfZ i c' t' e'
    go (Lam i n ty (Sc1 t)) = Lam i n ty (Sc1 (go t))
    go (App i t1 t2) = App i (go t1) (go t2)
    go (Fix i f fty x xty (Sc2 t)) = Fix i f fty x xty (Sc2 (go t))
    go (Pnt i l t) = Pnt i l (go t)
    go (Let i x xty alias (Sc1 body)) = Let i x xty (go alias) (Sc1 (go body))
    go t@(Var _ _) = t
    go t@(Lit _ _) = t

constantPropagation :: Decl TTerm -> Decl TTerm
constantPropagation dt = dt {body = go [] dt.body}
  where
    -- go :: [(Int, TTerm)] -> TTerm -> TTerm
    -- La lista es un ambiente: [(depth, valor_constante)]
    -- depth indica a qué índice Bound corresponde (relativo al nivel actual)

    go env (Var i (Bound n)) =
      case lookup n env of
        Just val -> val -- reemplazar por constante
        Nothing -> Var i (Bound n)
    -- Let con literal: agregar al ambiente para el cuerpo
    go env (Let i x xty l@(Lit _ _) (Sc1 body)) =
      -- Bound 0 en body corresponde a x, que vale l
      let env' = (0, l) : map (\(d, v) -> (d + 1, v)) env
          body' = go env' body
       in Let i x xty l (Sc1 body')
    -- Let sin literal: solo propagar en los subcampos
    go env (Let i x xty alias (Sc1 body)) =
      let alias' = go env alias
          -- Incrementar depths porque entramos en un nuevo scope
          env' = map (\(d, v) -> (d + 1, v)) env
          body' = go env' body
       in Let i x xty alias' (Sc1 body')
    -- Lambda: incrementar depths
    go env (Lam i n ty (Sc1 t)) =
      let env' = map (\(d, v) -> (d + 1, v)) env
       in Lam i n ty (Sc1 (go env' t))
    -- Fix: incrementar depths por 2 (dos variables ligadas)
    go env (Fix i f fty x xty (Sc2 t)) =
      let env' = map (\(d, v) -> (d + 2, v)) env
       in Fix i f fty x xty (Sc2 (go env' t))
    -- Recorrido recursivo para otros constructores
    go env (App i t1 t2) = App i (go env t1) (go env t2)
    go env (BOp i op t1 t2) = BOp i op (go env t1) (go env t2)
    go env (IfZ i c t e) = IfZ i (go env c) (go env t) (go env e)
    go env (Pnt i l t) = Pnt i l (go env t)
    -- Casos base (no se propagan constantes aquí)
    go env t@(Lit _ _) = t
    go env t@(Var _ (Free _)) = t
    go env t@(Var _ (Global _)) = t

-- Inline Expansion: beta-reducción de aplicaciones de lambdas
inLine :: Decl TTerm -> Decl TTerm
inLine dt = dt {body = go dt.body}
  where
    go :: TTerm -> TTerm

    -- Beta-reducción: (λx. body) arg -> body[x := arg]
    -- Solo con argumentos simples (Var o Lit)
    go (App i (Lam _ _ _ sc) arg@(Var _ _)) =
      go (subst arg sc)
    go (App i (Lam _ _ _ sc) arg@(Lit _ _)) =
      go (subst arg sc)
    -- App con otros términos: primero reducir subexpresiones
    go (App i t1 t2) =
      let t1' = go t1
          t2' = go t2
       in case t1' of
            Lam _ _ _ sc | isSimple t2' -> go (subst t2' sc)
            _ -> App i t1' t2'
    -- Recorrido recursivo
    go (Lam i n ty (Sc1 t)) = Lam i n ty (Sc1 (go t))
    go (BOp i op t1 t2) = BOp i op (go t1) (go t2)
    go (IfZ i c t e) = IfZ i (go c) (go t) (go e)
    go (Fix i f fty x xty (Sc2 t)) = Fix i f fty x xty (Sc2 (go t))
    go (Pnt i l t) = Pnt i l (go t)
    go (Let i x xty alias (Sc1 body)) = Let i x xty (go alias) (Sc1 (go body))
    -- Casos base
    go t@(Var _ _) = t
    go t@(Lit _ _) = t

    -- Helper: verifica si un término es "simple" (seguro de inline)
    isSimple :: TTerm -> Bool
    isSimple (Var _ _) = True
    isSimple (Lit _ _) = True
    isSimple _ = False

-- Da true si hay algún Print
hasEffects :: TTerm -> Bool
hasEffects (Lit _ _) = False
hasEffects (Pnt _ _ _) = True
hasEffects (Var _ _) = False
hasEffects (Lam _ n _ bdy) = hasEffects (open n bdy)
hasEffects (App _ f x) = hasEffects f || hasEffects x
hasEffects (Fix _ f _ x _ bdy) = hasEffects (open2 f x bdy)
hasEffects (BOp _ o x y) = hasEffects x || hasEffects y
hasEffects (IfZ _ c t f) = hasEffects c || hasEffects t || hasEffects f
hasEffects (Let _ x xty alias bdy) = hasEffects alias || hasEffects (open x bdy)

-- No debería ser necesario, pero se mantiene por compatibilidad
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

addReferences :: (MonadFD4 m) => TTerm -> m [TTerm]
addReferences t@(Var (i, ty) (Global n)) = do
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
  let noDeadDecls = filter (\d -> (not (mustBeFiltered d.body variables))) ds
  -- printFD4 $ "deadcode"
  return noDeadDecls

isGlobalVar :: TTerm -> Bool
isGlobalVar (Var (i, ty) (Global n)) = True
isGlobalVar _ = False

-- Devuelve True cuando es una variable global, no usada y sin efectos.
-- El código con efectos no podemos fletarlo, incluso aunque no se use.
mustBeFiltered :: TTerm -> [Name] -> Bool
mustBeFiltered t referredVariables = isGlobalVar t && not (isUsed t referredVariables) && not (hasEffects t)

isUsed :: TTerm -> [Name] -> Bool
isUsed (Var _ (Global n)) referredVariables = elem n referredVariables
isUsed _ _ = False