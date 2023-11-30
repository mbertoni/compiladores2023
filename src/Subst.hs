{-# LANGUAGE GADTs #-}
-- Ojo que esto es un anzuelo

-- |
-- Module      : Subst
-- Description : Define las operaciones de la representación locally nameless
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo define las operaciones de la representación locally nameless,
-- y la substitución.
module Subst where

import Common
import Core

-- Esta es una función auxiliar que usan el resto de las funciones de este módulo
-- para modificar las variables (ligadas y libres) de un término
varChanger ::
  (Int -> info -> Name -> Tm info Var) -> -- que hacemos con las variables localmente libres
  (Int -> info -> Int -> Tm info Var) -> -- que hacemos con los indices de De Bruijn
  Tm info Var ->
  Tm info Var
varChanger local bound term = go 0 term
  where
    go n = \case
      Lit p l -> Lit p l
      Var p (Bound i) -> bound n p i
      Var p (Free x) -> local n p x
      Var p (Global x) -> Var p (Global x)
      Lam p y ty (Sc1 t) -> Lam p y ty (Sc1 (go (n + 1) t))
      App p l r -> App p (go n l) (go n r)
      Fix p f fty x xty (Sc2 t) -> Fix p f fty x xty (Sc2 (go (n + 2) t))
      IfZ p c t e -> IfZ p (go n c) (go n t) (go n e)
      Pnt p str t -> Pnt p str (go n t)
      BOp p op t u -> BOp p op (go n t) (go n u)
      Let p v vty m (Sc1 o) -> Let p v vty (go n m) (Sc1 (go (n + 1) o))

-- `open nm t` reemplaza la primera variable ligada
-- en `t` (que debe ser un Scope con una sola variable que
-- escapa al término) por el nombre libre `nm`.
-- La variable Bound 0 pasa a ser Free nm. El nombre `nm`
-- debe ser fresco en el término para que no ocurra shadowing.
open :: Name -> Scope info Var -> Tm info Var
open nm (Sc1 t) = varChanger (\_ p n -> Var p (Free n)) bnd t
  where
    bnd depth p i
      | i < depth = Var p (Bound i)
      | i == depth = Var p (Free nm)
      | otherwise = abort "open: M is not LC"

-- `open2 n1 n2 t` reemplaza la primeras dos variables ligadas en `t`, que debe ser
-- un Scope con dos variables ligadas que escapan al término.
open2 :: Name -> Name -> Scope2 info Var -> Tm info Var
open2 nm1 nm2 (Sc2 t) = varChanger (\_ p n -> Var p (Free n)) bnd t
  where
    bnd depth p i
      | i < depth = Var p (Bound i)
      | i == depth = Var p (Free nm2)
      | i == depth + 1 = Var p (Free nm1)
      | otherwise = abort "open2: M is not LC"

-- `subst u t` sustituye el índice de de Bruijn 0 en t con
-- el término u (Bound 0 pasa a ser u). Notar que t es un Scope
-- con un solo índice que escapa el término.
-- Puede pensarse como una optimización de primero hacer `open
-- n t`, con nombres frescos, y luego sustituir los nombres
-- por los términos correspondientes. La ventaja es que no hace falta
-- generar ningún nombre, y por lo tanto evitamos la necesidad de
-- nombres frescos.
subst :: Tm info Var -> Scope info Var -> Tm info Var
subst n (Sc1 m) = varChanger (\_ p x -> Var p (Free x)) bnd m
  where
    bnd depth p i
      | i < depth = Var p (Bound i)
      | i == depth = n
      | otherwise = abort "substN: M is not LC"

substAll :: [Tm info Var] -> Tm info Var -> Tm info Var
substAll ns = varChanger (\_ p n -> Var p (Free n)) bnd
   where 
    nns = length ns
    bnd depth p i
      | i <  depth = Var p (Bound i)
      | i >= depth && i < depth + nns = ns !! (i - depth)
      | otherwise = abort "substAll: M is not LC"

-- `subst2 u1 u2 t1 sustituye índice de de Bruijn 0 en t por u1 y el índice 1 por u2.
-- Notar que t es un Scope con dos índices que escapan el término.
subst2 :: Tm info Var -> Tm info Var -> Scope2 info Var -> Tm info Var
subst2 n1 n2 (Sc2 m) = varChanger (\_ p n -> Var p (Free n)) bnd m
  where
    bnd depth p i
      | i < depth = Var p (Bound i)
      | i == depth = n2
      | i == depth + 1 = n1
      | otherwise = abort "substN: M is not LC"

-- `close n t` es la operación inversa a open. Reemplaza
-- las variables `Free n` por la variable ligada `Bound 0`.
close :: Name -> Tm info Var -> Scope info Var
close nm t = Sc1 (varChanger lcl (\_ p i -> Var p (Bound i)) t)
  where
    lcl depth p y =
      if y == nm
        then Var p (Bound depth)
        else Var p (Free y)

-- Similar a `close` pero para el caso de cerrar dos nombres.
close2 :: Name -> Name -> Tm info Var -> Scope2 info Var
close2 nm1 nm2 t = Sc2 (varChanger lcl (\_ p i -> Var p (Bound i)) t)
  where
    lcl depth p y
      | y == nm2 = Var p (Bound depth)
      | y == nm1 = Var p (Bound (depth + 1))
      | otherwise = Var p (Free y)
