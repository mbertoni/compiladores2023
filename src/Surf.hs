{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Surf where

import Common (Pos)

type Ident = String

data Literal = N Int | S String
  deriving (Show)

data UnaryOp = Bang
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

data Rec binder = Rec binder | NoRec
  deriving (Show)

data Par = Par | NoPar
  deriving (Show)

type Bind symbol referent = ([symbol], referent)

data Decl ident binder ty term
  = TypeDecl ident ty
  | LetDecl Par ident (Rec binder) [binder] ty term
  deriving (Show)

-- \| AST the términos superficiales
data Tm ident binder ty term
  = Var ident
  | Lit Literal
  | Pnt String term
  | UOp UnaryOp term
  | BOp BinaryOp term term
  | IfZ term term term
  | App term term
  | Lam [binder] ty term
  | Fix binder binder [binder] ty term
  | Let Par ident (Rec binder) [binder] ty term term
  deriving (Show, Functor)

-- me lleva el chango, en ocaml
type Binder = Bind Ident Ty

newtype Term = T {unT :: Tm Ident Binder Ty Term}

type Declaration = Decl Ident Binder Ty Term

data Ty
  = Nat
  | Arrow Ty Ty
  | Alias Ident
  deriving (Show, Eq)

tyFold :: [Ty] -> Ty
tyFold = foldr1 Arrow
