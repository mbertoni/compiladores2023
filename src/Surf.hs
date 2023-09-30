{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Surf where

import Common (abort)
import Data.List.NonEmpty
import Data.String (IsString (..))

data Ident
  = VarId {unVarId :: String}
  | TyId {unTyId :: String}
  deriving (Eq)

data Literal
  = N {unN :: Integer}
  | S {unS :: String}
  deriving (Eq)

data UnaryOp = Bang
  deriving (Eq)

data BinaryOp = Add | Sub
  deriving (Eq)

data Rec binder = NoRec | Rec binder
  deriving (Functor)

data Par = P | NP
  deriving (Eq, Show)

type Bind = (,)

bind :: a -> b -> Bind a b
bind = (,)

type Binder = Bind Ident Ty

type Multi = Bind (NonEmpty Ident) Ty -- TODO los multi tienen que tener 2 variables

data Decl term
  = TypeDecl Binder
  | LetDecl Par Binder (Rec Multi) [Multi] term

type Declaration = Decl Term


-- \| AST the términos superficiales
data Tm term
  = Var Ident
  | Par term
  | Lit Literal
  | Pnt Literal term
  | UOp UnaryOp term
  | BOp BinaryOp term term
  | IfZ term term term
  | App term term
  | Fun (NonEmpty Multi) term
  | Fix Binder Binder [Multi] term -- : y si viene un multi con dos variables (f y x)?
  | Let Par Binder (Rec Multi) [Multi] term term
  -- falta ver el comentario en ss.pdf del print parcialmente aplicado
  deriving (Functor)

newtype Term = T {unT :: Tm Term}

data Ty
  = Nat
  | ParTy Ty
  | Arrow Ty Ty
  | Alias Ident
  deriving (Eq)

tyFold :: [Ty] -> Ty
tyFold = foldr1 Arrow

-- Instancias para abreviar cuando depuramos
instance Num Literal where
  fromInteger = N . fromInteger

instance Num (Tm t) where
  fromInteger = Lit . fromInteger

deriving instance Num Term

instance IsString Literal where
  fromString = S

instance IsString (Tm t) where
  fromString = Lit . fromString

deriving instance IsString Term

-- deriving instance Show Ident
instance Show Ident where
  show = \case
    VarId s -> s
    TyId s -> s

-- deriving instance Show Literal
instance Show Literal where
  show = \case
    N n -> show n
    S s -> show s

-- deriving instance Show UnaryOp
instance Show UnaryOp where
  show Bang = "!"

-- deriving instance Show BinaryOp
instance Show BinaryOp where
  show = \case
    Add -> "+"
    Sub -> "-"

-- deriving instance Show Term
instance Show Term where show = show . unT

-- deriving instance Show Ty
instance Show Ty where
  show = \case
    Nat -> "__Nat__"
    ParTy t -> "(" <> show t <> ")"
    Arrow t t' -> "|" <> show t <> " -> " <> show t' <> "|"
    Alias n -> show n

deriving instance (Show a) => Show (Rec a)

deriving instance (Show t) => Show (Decl t)

deriving instance (Show t) => Show (Tm t)
