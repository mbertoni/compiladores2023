{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Typechecker
-- Description : Chequeo de tipos de términos y declaraciones.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module TypeChecker (tc, tcDecl) where

import Core
import Global
import MonadFD4
import PPrint
import Subst

-- | 'tc' chequea y devuelve el tipo de un término
-- Si el término no está bien tipado, lanza un error
-- usando la interfaz de las mónadas @MonadFD4@.
tc ::
  (MonadFD4 m) =>
  -- | término a chequear
  Term ->
  -- | entorno de tipado
  [(Name, Ty)] ->
  -- | tipo del término
  m TTerm
tc (Var p (Bound _)) _ =
  failPosFD4 p "typecheck: No debería haber variables Bound"
tc (Var p (Free n)) bs = case lookup n bs of
  Nothing -> failPosFD4 p $ "Variable no declarada " ++ ppName n
  Just ty -> return (Var (p, ty) (Free n))
tc (Var p (Global n)) bs = case lookup n bs of
  Nothing -> failPosFD4 p $ "Variable no declarada " ++ ppName n
  Just ty -> return (Var (p, ty) (Global n))
tc (Lit p (N n)) _ = return (Lit (p, Nat) (N n))
tc (Pnt p str t) bs = do
  tt <- tc t bs
  expect Nat tt
  return (Pnt (p, Nat) str tt)
tc (IfZ p c t t') bs = do
  ttc <- tc c bs
  expect Nat ttc
  tt <- tc t bs
  tt' <- tc t' bs
  let ty = getTy tt
  expect ty tt'
  return (IfZ (p, ty) ttc tt tt')
tc (Lam p v ty t) bs = do
  tt <- tc (open v t) ((v, ty) : bs)
  return (Lam (p, Arrow ty (getTy tt)) v ty (close v tt))
tc (App p t u) bs = do
  tt <- tc t bs
  (dom, cod) <- domCod tt
  tu <- tc u bs
  expect dom tu
  return (App (p, cod) tt tu)
tc (Fix p f fty x xty t) bs = do
  (dom, cod) <- domCod (Var (p, fty) (Free f))
  when (dom /= xty) $
    do
      failPosFD4
        p
        "El tipo del argumento de un fixpoint debe coincidir con el \
        \dominio del tipo de la función"
  let t' = open2 f x t
  tt' <- tc t' ((x, xty) : (f, fty) : bs)
  expect cod tt'
  return (Fix (p, fty) f fty x xty (close2 f x tt'))
tc (Let p v ty def t) bs = do
  tdef <- tc def bs
  expect ty tdef
  tt <- tc (open v t) ((v, ty) : bs)
  return (Let (p, getTy tt) v ty tdef (close v tt))
tc (BOp p op t u) bs = do
  tt <- tc t bs
  expect Nat tt
  tu <- tc u bs
  expect Nat tu
  return (BOp (p, Nat) op tt tu)

-- | @'typeError' t s@ lanza un error de tipo para el término @t@
typeError ::
  (MonadFD4 m) =>
  -- | término que se está chequeando
  TTerm ->
  -- | mensaje de error
  String ->
  -- | no retorna, devuelve forall a
  (forall a. m a)
typeError t s = do
  ppt <- pp t
  failPosFD4 (getPos t) $ "Error de tipo en " ++ ppt ++ "\n" ++ s

-- | 'expect' chequea que el tipo esperado sea igual al que se obtuvo
-- y lanza un error si no lo es.
expect :: (MonadFD4 m) => Ty -> TTerm -> m TTerm -- esta es la que tendríamos que modificar para no fallar con los synonyms
expect ty tt =
  let ty' = getTy tt
  in if ty == ty'
      then return tt
      else
        typeError tt $
          "Tipo esperado: " ++ ppTy ty ++ "\npero se obtuvo: " ++ ppTy ty'

-- | 'domCod chequea que un tipo sea función
-- | devuelve un par con el tipo del dominio y el co-dominio de la función
domCod :: (MonadFD4 m) => TTerm -> m (Ty, Ty)
domCod tt = case getTy tt of
  Arrow d c -> return (d, c)
  _ ->
    typeError tt $
      "Se esperaba un tipo función, pero se obtuvo: " ++ ppTy (getTy tt)

-- | 'tcDecl' chequea el tipo de una declaración
tcDecl :: (MonadFD4 m) => Decl Term -> m (Decl TTerm)
tcDecl (Decl p n t) = do
  -- chequear si el nombre ya está declarado
  mty <- lookupTypeOfGlobal n
  case mty of
    Nothing ->
      -- no está declarado
      do
        s <- get
        tt <- tc t (globalTypedEnvironment s)
        return (Decl p n tt)
    Just _ -> failPosFD4 p $ n ++ " ya está declarado"
