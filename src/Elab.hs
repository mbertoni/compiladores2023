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
module Elab where

import Common (abort)
import Control.Monad
import Core
import Data.Bifunctor (Bifunctor (bimap))
import Data.Default
import Data.List.NonEmpty
import Data.Maybe
import Subst
import Surf qualified as S

-- | 'go transforma variables ligadas en índices de de Bruijn
-- en un término dado.
term :: [Binder] -> S.Term -> Term
term gamma = term' []
  where
    ty' :: S.Ty -> Ty
    ty' = ty gamma

    binder' = binder gamma

    multi' = multi gamma

    term' :: [Name] -> S.Term -> Term
    term' locals (S.T tm) = case tm of
      -- Tenemos que ver si la variable es Global o es un nombre local
      -- En locals llevamos la lista de nombres locales.
      S.Var i ->
        let nm = ident i
        in if nm `elem` locals
            then Var def (Free nm)
            else Var def (Global nm)
      S.Lit l -> Lit def (literal l)
      S.Pnt l t -> Pnt def (literal l) (rec t)
      S.UOp op t -> case op of
        S.Bang -> IfZ def (rec t) 1 0
      S.BOp op t u -> BOp def (binaryOp op) (rec t) (rec u)
      S.IfZ c t e -> IfZ def (rec c) (rec t) (rec e)
      S.App f x -> App def (rec f) (rec x)
      S.Fun bs t -> f locals (toList bs >>= multi')
        where
          -- TODO: f :: [Name] -> NonEmpty Binder -> Term
          -- el env de local se pasa con foldl
          -- el term de lam anidadas con foldr
          -- se puede separar en dos pasadas, una que elab y la otra que
          -- pasa a locally closed.
          -- Se puede hacer tuplin? Ver el último libro de bird );
          f :: [Name] -> [Binder] -> Term
          f e [] = term' e t
          f e ((x, tau) : xts) = Lam def x tau (close x (f (x : e) xts))
      S.Fix f x bs t -> Fix def f' tf' x' tx' scope2
        where
          (f', tf') = binder' f
          (x', tx') = binder' x
          scope2 = close2 f' x' (rec t')
          t' = case bs of
            [] -> t
            _ -> S.T $ S.Fun (fromList bs) t
      _ -> abort "wip"
      where
        rec = term' locals

{-
      S.Lam [(v, ty)] t -> Lam p v (go' ty) (close v (go (v : env) t))
      S.Lam ((v, ty) : bs) t ->
        Lam v (go' ty) (close v (go (v : env) (S.Lam p bs t)))
      S.Fix (f, fty) [(x, xty)] t ->
        Fix f (go' fty) x (go' xty) (close2 f x (go (x : f : env) t))
      S.Fix (f, fty) ((x, xty) : bs) t ->
        Fix f (go' fty) x (go' xty) (close2 f x (go (x : f : env) (S.Lam i bs t)))
      S.Let (fn, ty) bs t t' ->
        go env (S.Let i (fn, funTy) (S.Lam i bs t) t')
        where
          funTy = S.tyFold (map snd bs ++ [ty])

      -- Operadores binarios

      -- Operador Print
      -- Aplicaciones generales

      S.Let (v, vty) def bdy ->
        Let def v (go' vty) (go env def) (close v (go (v : env) bdy))
      S.Let (f, ty) (b : bs) t t' ->
        go env (S.Let (f, funTy) (S.Fix (f, funTy) (b : bs) t) t')
        where
          funTy = S.tyFold (map snd (b : bs) ++ [ty])
-}

ident :: S.Ident -> Name
ident = \case
  S.VarId s -> s
  S.TyId s -> s

binder :: [Binder] -> S.Binder -> Binder
binder gamma = bimap ident (ty gamma)

multi :: [Binder] -> S.Multi -> [Binder]
multi gamma (is, tau) = toList $ fmap (\i -> (ident i, ty gamma tau)) is

ty :: [Binder] -> S.Ty -> Ty
ty gamma = \case
  S.Nat -> Nat
  S.ParTy t -> r t
  S.Arrow t t' -> Arrow (r t) (r t')
  S.Alias n -> fromMaybe (abort "alias no definido") (lookup (ident n) gamma)
  where
    r = ty gamma

literal :: S.Literal -> Literal
literal = \case
  S.N n -> N $ fromInteger n
  S.S s -> S s

binaryOp :: S.BinaryOp -> BinaryOp
binaryOp S.Add = Add
binaryOp S.Sub = Sub

tyFold :: [Ty] -> Ty
tyFold = foldr1 Arrow

{-
elabDeclaration :: [(Name, Ty)] -> S.Declaration -> Decl (Either Term Ty)
elabDeclaration types decl =
  Decl {name = S.name decl, pos = S.pos decl, body = elaboratedBody}
  where
    elaboratedBody = case S.body decl of
      S.LetDecl sTerm -> Left $ elabTerm types sTerm
      S.TypeDecl sType -> Right $ elabType types sType
-}
