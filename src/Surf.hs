{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Surf where

import Data.String (IsString (..))

type Ident = String

data Literal
  = N {unN :: Integer}
  | S {unS :: String}
  deriving (Show)

data UnaryOp = Bang
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

data Rec binder = Rec binder | NRec
  deriving (Show)

data Par = P | NP
  deriving (Eq, Show)

type Bind = (,)
bind :: a -> b -> Bind a b
bind = (,)

type Multi a b = [Bind [a] b]
data Decl binder multi term
  = TypeDecl binder
  | LetDecl Par binder (Rec binder) multi term
  deriving (Show)

-- \| AST the términos superficiales
data Tm binder multi ident term
  = Var ident
  | Par term
  | Lit Literal
  | Pnt Literal term
  | UOp UnaryOp term
  | BOp BinaryOp term term
  | IfZ term term term
  | App term term
  | Lam multi term
  | Fix binder binder multi term
  | Let Par binder (Rec binder) multi term term
  -- falta ver el comentario en ss.pdf del print parcialmente aplicado
  deriving (Show, Functor)

type Binder = Bind Ident Ty
type MultiBinder = Multi Ident Ty

newtype Term = T {unT :: Tm Binder MultiBinder Ident Term}

type Declaration = Decl Binder MultiBinder Term

data Ty
  = Nat
  | ParTy Ty
  | Arrow Ty Ty
  | Alias Ident
  deriving (Show, Eq)

tyFold :: [Ty] -> Ty
tyFold = foldr1 Arrow

-- Instancias para abreviar cuando depuramos
instance Num Literal where
  fromInteger = N . fromInteger

instance Num (Tm a b c d) where
  fromInteger = Lit . fromInteger

instance IsString Literal where
  fromString = S

instance IsString (Tm a b c d) where
  fromString = Lit . fromString
