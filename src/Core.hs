{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Core
-- Description : AST de términos, declaraciones y tipos
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Definiciones de distintos tipos de datos:
--   - AST de términos
--   - Declaraciones
--   - Tipos
--   - Variables
module Core where

import Common
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Default
import Data.List.Extra
import Data.String
import Data.Traversable

type Name = String

type Binder = (Name, Ty)

data Literal
  = N {unN :: Int}
  | S {unS :: String}
  | U {unU :: ()}
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a = Decl
  { pos :: Pos,
    name :: Name,
    body :: a
  }
  deriving (Show, Functor)

-- | AST de Tipos
data Ty
  = Named Name -- Llevo alias, o expando, o lo dejo en la info
  | Nat -- TODO se pueden sacar y los agregasmos al contexto inicial
  | String -- TODO se pueden sacar
  | Unit -- TODO se pueden sacar
  | Arrow Ty Ty
  deriving (Show, Eq)

-- | AST de los términos.
--   - info es información extra que puede llevar cada nodo.
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed.
data Tm info var
  = Var info var
  | Lit info Literal
  | Lam info Name Ty (Scope info var)
  | App info (Tm info var) (Tm info var)
  | Pnt info Literal (Tm info var)
  | BOp info BinaryOp (Tm info var) (Tm info var)
  | Fix info Name Ty Name Ty (Scope2 info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var) (Scope info var)
  deriving (Show, Functor, Foldable, Traversable, Bifunctor, Bifoldable, Bitraversable)

-- | 'Tm' con índices de De Bruijn como variables ligadas, y nombres para libres y globales, guarda posición
type Term = Tm Pos Var

-- | 'Tm' con índices de De Bruijn como variables ligadas, y nombres para libres y globales, guarda posición y tipo
type TTerm = Tm (Pos, Ty) Var

type Module = [Decl TTerm] -- Representa un archivo de FD4

data Var
  = Bound !Int
  | Free Name
  | Global Name
  deriving (Show)

-- Scope es un término con una o dos variables que escapan.
newtype Scope info var = Sc1 {unSc1 :: Tm info var}
  deriving newtype (Functor, Bifunctor, Foldable)
  deriving stock (Traversable)

newtype Scope2 info var = Sc2 {unSc2 :: Tm info var}
  deriving newtype (Functor, Bifunctor, Foldable)
  deriving stock (Traversable)

instance (Show info, Show var) => Show (Scope info var) where
  show (Sc1 t) = "{" ++ show t ++ "}"

instance (Show info, Show var) => Show (Scope2 info var) where
  show (Sc2 t) = "{{" ++ show t ++ "}}"

-- Instancias para abreviar cuando depuramos
instance Num Literal where
  fromInteger = N . fromInteger

instance (Default a) => Num (Tm a b) where
  fromInteger = Lit def . fromInteger

instance IsString Literal where
  fromString = S

instance (Default a) => IsString (Tm a b) where
  fromString = Lit def . fromString

instance Default Ty where
  def = Nat -- Named "Unit"

instance Default Literal where
  def = U ()

instance (Default info) => Default (Tm info var) where
  def = Lit def def

-- | Obtiene la info en la raíz del término.
getInfo' :: (_) => Tm info var -> info -- instance monoid pos hecha a la derecha, los tipos con subtyping... TODO 2031
getInfo' = bifoldMap id (const mempty) -- ver bi-traversable

-- | TODO Unlawful! Es para probar
instance Semigroup Ty where
  (<>) = Arrow -- NO es asociativo

-- | TODO Unlawful! Es para probar
instance Monoid Ty where
  mempty = Unit

getInfo :: Tm info var -> info
getInfo (Var i _) = i
getInfo (Lit i _) = i
getInfo (Lam i _ _ _) = i
getInfo (App i _ _) = i
getInfo (Pnt i _ _) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i
getInfo (Let i _ _ _ _) = i
getInfo (BOp i _ _ _) = i

getTerm :: TTerm -> Term
getTerm (Var i v) = Var (fst i) v
getTerm (Lit i l) = Lit (fst i) l
getTerm (Lam i1 n t s) = Lam (fst i1) n t (ts2s s)
getTerm (App i t1 t2) = App (fst i) (getTerm t1) (getTerm t2)
getTerm (Pnt i l t) = Pnt (fst i) l (getTerm t)
getTerm (Fix i fn ft xn xt s) = Fix (fst i) fn ft xn xt (ts22s s)
getTerm (IfZ i c t f) = IfZ (fst i) (getTerm c) (getTerm t) (getTerm f)
getTerm (Let i nm ty t s) = Let (fst i) nm ty (getTerm t) (ts2s s)
getTerm (BOp i op t1 t2) = BOp (fst i) op (getTerm t1) (getTerm t2)

ts2s (Sc1 t) = Sc1 (getTerm t)
ts22s (Sc2 t) = Sc2 (getTerm t)

getTy :: TTerm -> Ty
getTy = snd . getInfo

getPos :: TTerm -> Pos
getPos = fst . getInfo


-- | map para la info de un término
mapInfo :: (a -> b) -> Tm a var -> Tm b var
mapInfo = first

{-
mapInfo :: (a -> b) -> Tm a var -> Tm b var
mapInfo f (Var i x) = Var (f i) x
mapInfo f (Lit i x) = Lit (f i) x
mapInfo f (Lam i x ty (Sc1 y)) = Lam (f i) x ty (Sc1 $ mapInfo f y)
mapInfo f (App i x y) = App (f i) (mapInfo f x) (mapInfo f y)
mapInfo f (Pnt i msg y) = Pnt (f i) msg (mapInfo f y)
mapInfo f (BOp i x y z) = BOp (f i) x (mapInfo f y) (mapInfo f z)
mapInfo f (Fix i x xty y yty (Sc2 z)) = Fix (f i) x xty y yty (Sc2 $ mapInfo f z)
mapInfo f (IfZ i x y z) = IfZ (f i) (mapInfo f x) (mapInfo f y) (mapInfo f z)
mapInfo f (Let i x xty y (Sc1 z)) = Let (f i) x xty (mapInfo f y) (Sc1 $ mapInfo f z)
-}

-- | Obtiene los nombres de variables (abiertas o globales) de un término.
freeVars :: Tm info Var -> [Name]
freeVars tm = nubSort $ go tm []
  where
    go (Var _ (Free v)) xs = v : xs
    go (Var _ (Global v)) xs = v : xs
    go (Var _ _) xs = xs
    go (Lam _ _ _ (Sc1 t)) xs = go t xs
    go (App _ l r) xs = go l $ go r xs
    go (Pnt _ _ t) xs = go t xs
    go (BOp _ _ t u) xs = go t $ go u xs
    go (Fix _ _ _ _ _ (Sc2 t)) xs = go t xs
    go (IfZ _ c t e) xs = go c $ go t $ go e xs
    go (Lit _ _) xs = xs
    go (Let _ _ _ e (Sc1 t)) xs = go e (go t xs)

class Fremen a where
  fre :: a -> [Name] -> [Name] -- d-list?

instance Fremen Var where
  fre = \case
    Free n -> (n :)
    Global n -> (n :)
    _ -> id

deriving newtype instance (Fremen bae) => Fremen (Scope info bae)

deriving newtype instance (Fremen bae) => Fremen (Scope2 info bae)

instance (Fremen bae) => Fremen (Tm info bae) where
  fre _ = id -- TODO
