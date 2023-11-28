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

import Common
import Core
import Data.Bifunctor
import Data.Default
import Data.List.NonEmpty
import Data.Maybe
import Subst
import Surf qualified as S

-- | 'term' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
term :: [Binder] -> S.Term -> Term
term gamma = goTerm []
  where
    goTy :: S.Ty -> Ty
    goTy = ty gamma

    goBinder :: S.Binder -> Binder
    goBinder = binder gamma

    goMulti :: S.Multi -> [Binder]
    goMulti = multi gamma

    goTerm :: [Name] -> S.Term -> Term
    goTerm locals (S.T tm) =
      let go = goTerm locals
      in case tm of
          -- Tenemos que ver si la variable es Global o es un nombre local
          -- En locals llevamos la lista de nombres locales.
          S.Var i ->
            let nm = ident i
            in if nm `elem` locals
                then Var def (Free nm)
                else Var def (Global nm)
          S.Par t -> go t
          S.Lit l -> Lit def (literal l)
          S.Pnt l t -> Pnt def (literal l) (go t)
          S.UOp op t -> case op of
            S.Bang -> IfZ def (go t) 1 0
          S.BOp op t1 t2 -> BOp def (binaryOp op) (go t1) (go t2)
          S.IfZ c t e -> IfZ def (go c) (go t) (go e)
          S.App f x -> App def (go f) (go x)
          S.Fun xs t -> f locals (toList xs >>= goMulti)
            where
              f :: [Name] -> [Binder] -> Term
              f locs [] = goTerm locs t
              f locs ((x, tau) : xts) = Lam def x tau (close x $ f (x : locs) xts)
          -- TODO: f :: [Name] -> NonEmpty Binder -> Term
          -- el env de local se pasa con foldl
          -- el term de lam anidadas con foldr
          -- se puede separar en dos pasadas, una que elab y la otra que
          -- pasa a locally closed?
          -- Se puede hacer tupling? Ver el último libro de bird );
          S.Fix f x xs t -> Fix def _f tau_f _x tau_x sc2
            where
              sc2 = close2 _f _x _t
              (_f, tau_f) = goBinder f
              (_x, tau_x) = goBinder x
              _t = case xs of
                [] -> go t
                _ -> go . S.T $ S.Fun (fromList xs) t
          S.Let p f S.NoRec xs t t' ->
            case xs of
              [] -> Let def _f tau (go t) (close _f (goTerm (_f : locals) t'))
              _ -> Let def _f tau fun (close _f (goTerm (_f : locals) t'))
            where
              tau = foldr (Arrow . snd) tau_f (xs >>= goMulti)
              fun = go . S.T $ S.Fun (fromList xs) t
              (_f, tau_f) = goBinder f
          S.Let p f (S.Rec xs) ys t t' ->
            let x :| xs' = S.flatten xs
                args = xs' <> (ys >>= toList . S.flatten)
            in case args of
                [] ->
                  Let def _f tau fix (close _f (goTerm (_f : locals) t'))
                  where
                    tau = Arrow tau_x tau_f
                    fix = Fix def _f tau _x tau_x (close2 _f _x (goTerm (_f : _x : locals) t))
                    (_f, tau_f) = goBinder f
                    (_x, tau_x) = goBinder x
                _ ->
                  go . S.T $ S.Let p f' x' [] fun t'
                  where
                    f' = S.bind (fst f) (foldr (S.Arrow . snd) (snd f) args)
                    x' = S.Rec $ first pure x
                    fun = S.T $ S.Fun (first pure <$> fromList args) t

ident :: S.Ident -> Name
ident x = case x of
  S.VarId s -> s
  S.TyId s -> s

binder :: [Binder] -> S.Binder -> Binder
binder gamma = bimap ident (ty gamma)

multi :: [Binder] -> S.Multi -> [Binder]
multi gamma (is, tau) = toList $ fmap (\i -> (ident i, ty gamma tau)) is

ty :: [Binder] -> S.Ty -> Ty
ty gamma = \case
  S.Nat -> Nat
  S.ParTy t -> go t
  S.Arrow t t' -> Arrow (go t) (go t')
  S.Alias n -> let _n = ident n in fromMaybe (Named _n) (lookup _n gamma)
  where
    go = ty gamma

literal :: S.Literal -> Literal
literal = \case
  S.N n -> N $ fromInteger n
  S.S s -> S s

binaryOp :: S.BinaryOp -> BinaryOp
binaryOp = \case
  S.Add -> Add
  S.Sub -> Sub

-- TODO sacar de las declaraciones de términos la info que necesitamos para agrandar el
-- entorno global.
-- TODO  Either (Decl Term) (Decl Ty) \cong Decl (Either Term Ty)
-- ahora está roto en el driver
declaration :: [Binder] -> S.Declaration -> Either (Decl Term) (Decl Ty)
declaration gamma = \case
  S.TypeDecl b ->
    Right
      $ Decl
        { name = ident $ fst b,
          pos = def,
          body = _body
        }
    where
      _body = ty gamma $ snd b
  S.LetDecl p f r xs t ->
    Left
      $ Decl
        { name = ident $ fst f,
          pos = def,
          body = _body
        }
    where
      _body = case term gamma (S.T $ S.Let p f r xs t def) of
        Let _ _ _ tm _ -> tm
        _ -> abort "let decl elab"



