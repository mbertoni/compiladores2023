{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE DeriveAnyClass #-}

module Surf where

import Common (Pos)

type Ident = String

data Literal = N Integer | S String
  deriving (Show)

data UnaryOp = Bang
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

data Rec binder = Rec binder | NoRec
  deriving (Show)

data Par = P | NoP
  deriving (Eq, Show)

type Bind symbol referent = ([symbol], referent)

data Decl ident binder ty term
  = TypeDecl ident ty
  | LetDecl Par ident (Rec binder) [binder] ty term
  deriving (Show)

-- \| AST the términos superficiales
data Tm ident binder ty term
  = Var ident
  | Par term
  | Lit Literal
  | Pnt String term
  | UOp UnaryOp term
  | BOp BinaryOp term term
  | IfZ term term term
  | App term term
  | Lam [binder] ty term
  | Fix binder binder [binder] ty term
  | Let Par ident (Rec binder) [binder] ty term term
  -- falta ver el comentario en ss.pdf del print parcialmente aplicado
  deriving (Show, Functor)

-- me lleva el chango, en ocaml
type Binder = Bind Ident Ty

newtype Term = T {unT :: Tm Ident Binder Ty Term}

type Declaration = Decl Ident Binder Ty Term

data Ty
  = Nat
  | ParTy Ty
  | Arrow Ty Ty
  | Alias Ident
  deriving (Show, Eq)

tyFold :: [Ty] -> Ty
tyFold = foldr1 Arrow
