{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Surf where
import Common (Pos)

type Name = String

data Decl a = Decl
  { pos :: Pos,
    name :: Name,
    body :: a
  }
  deriving (Show, Functor)

data DeclBody
  = TypeDecl Ty
  | TermDecl Term
  deriving (Show)

-- | Estas son las declaraciones válidas del FD4
type Declaration = Decl DeclBody

data UnaryOp = Bang
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

type Binding from to = (from, to)

type Bindings from to = [Binding from to]

-- | AST the términos superficiales
data Tm info ty var
  =
  Var info var
  | Lit info Integer
  | Lam info (Bindings var ty) (Tm info ty var)
  | App info (Tm info ty var) (Tm info ty var)
  | Pnt info String (Tm info ty var)
  | BOp info BinaryOp (Tm info ty var) (Tm info ty var)
  | UOp info UnaryOp (Tm info ty var)
  | Fix info (var, ty) [(var, ty)] (Tm info ty var)
  | IfZ info (Tm info ty var) (Tm info ty var) (Tm info ty var)
  | If info [(Tm info ty var, Tm info ty var)]
  | Let info (var, ty) (Tm info ty var) (Tm info ty var)
  | LetRec info (var, ty) [(var, ty)] (Tm info ty var) (Tm info ty var)
  | LetFun info (var, ty) [(var, ty)] (Tm info ty var) (Tm info ty var)
  deriving (Show, Functor)

type Term = Tm Pos Ty Name

data Ty
  = Nat
  | Arrow Ty Ty
  | Alias Name
  deriving (Show, Eq)

tyFold :: [Ty] -> Ty
tyFold = foldr1 Arrow
