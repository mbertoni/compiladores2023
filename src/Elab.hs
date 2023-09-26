-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Elab
-- Description : Elabora un término fully named a uno locally closed.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite elaborar términos y declaraciones para convertirlas desde
-- fully named (@STerm) a locally closed (@Term@)
module Elab (elabDeclaration, elabTerm) where

import Common (abort)
import Core
import Data.Maybe
import Subst
import qualified Surf as S
import Data.String (IsString (..))


-- | 'go transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elabTerm :: [(Name, Ty)] -> S.Term -> Term
elabTerm types = go []
  where
    go' :: S.Ty -> Ty
    go' = elabType types

    go :: [Name] -> S.Term -> Term
    go env = \case
      S.Var p v ->
        -- Tenemos que ver si la variable es Global o es un nombre local
        -- En env llevamos la lista de nombres locales.
        if v `elem` env
          then Var p (Free v)
          else Var p (Global v)
      S.Lit p int -> Lit p (fromInteger int)
      S.Lam p [] t -> abort "Empty lambda binding list"
      S.Lam p [(v, ty)] t -> Lam p v (go' ty) (close v (go (v : env) t))
      S.Lam p ((v, ty) : bs) t ->
        Lam p v (go' ty) (close v (go (v : env) (S.Lam p bs t)))
      S.Fix i (f, fty) [] t -> abort "Empty fix binding list"
      S.Fix i (f, fty) [(x, xty)] t ->
        Fix i f (go' fty) x (go' xty) (close2 f x (go (x : f : env) t))
      S.Fix i (f, fty) ((x, xty) : bs) t ->
        Fix i f (go' fty) x (go' xty) (close2 f x (go (x : f : env) (S.Lam i bs t)))
      S.IfZ p c t e -> IfZ p (go env c) (go env t) (go env e)
      -- des hardcodear el Bang
      S.LetFun i (fn, ty) bs t t' ->
        go env (S.Let i (fn, funTy) (S.Lam i bs t) t')
        where
          funTy = S.tyFold (map snd bs ++ [ty])
      S.UOp i S.Bang t ->
        IfZ i (go env t) (Lit i (N 1)) (Lit i (N 0))
      S.If i _ -> abort "unimplemented"
      -- Operadores binarios
      S.BOp i o t u -> BOp i (elabBOp o) (go env t) (go env u)
      -- Operador Print
      S.Pnt i str t -> Pnt i (fromString str) (go env t)
      -- Aplicaciones generales
      S.App p h a -> App p (go env h) (go env a)
      S.Let p (v, vty) def bdy ->
        Let p v (go' vty) (go env def) (close v (go (v : env) bdy))
      S.LetRec i (f, ty) [] t t' -> abort "Empty let rec list"
      S.LetRec i (f, ty) (b : bs) t t' ->
        go env (S.Let i (f, funTy) (S.Fix i (f, funTy) (b : bs) t) t')
        where
          funTy = S.tyFold (map snd (b : bs) ++ [ty])

elabType :: [(Name, Ty)] -> S.Ty -> Ty
elabType types = \case
  S.Nat -> Nat
  S.Arrow t t' -> Arrow (elabType types t) (elabType types t')
  S.Alias n -> fromMaybe (abort "alias no definido") (lookup n types)


elabBOp :: S.BinaryOp -> BinaryOp
elabBOp S.Add = Add
elabBOp S.Sub = Sub

elabDeclaration :: [(Name, Ty)] -> S.Declaration -> Decl (Either Term Ty)
elabDeclaration types decl =
  Decl {name = S.name decl, pos = S.pos decl, body = elaboratedBody}
  where
    elaboratedBody = case S.body decl of
      S.TermDecl sTerm -> Left $ elabTerm types sTerm
      S.TypeDecl sType -> Right $ elabType types sType
