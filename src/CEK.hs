module CEK (eval) where

import Common
import Core
import Data.Default (def)
import Eval (semOp)
import MonadFD4

data Value
  = VNat Int
  | CFun Env Name TTerm
  | CFix Env Name Name TTerm
  deriving (Show)

lit2Value :: Literal -> Value
lit2Value (N i) = VNat i
lit2Value (S s) = abort "not implemented"

val2TTerm :: Value -> TTerm
val2TTerm (VNat i) = Lit (def, Nat) (N i)
val2TTerm _ = abort "immediate value expected"

type Env = [Value]

type Continuation = [Frame]

data Frame
  = AppL Env TTerm -- (App □ arg)
  | AppR Value -- (App f □)
  | IfZC Env TTerm TTerm -- (IfZ □ then else)
  | BOpL Env BinaryOp TTerm -- (□ (+) u)
  | BOpR BinaryOp Value -- (v (+) □)
  | -- | VarT Var
    PntT Literal -- (print s □)
  | LetD Env Name TTerm -- let □ in term
  deriving (Show)

seek :: (MonadFD4 m) => TTerm -> Env -> Continuation -> m Value
seek term env k = case term of
  Pnt _ s t -> seek t env (PntT s : k)
  BOp _ op t u -> seek t env (BOpL env op u : k)
  IfZ _ c t e -> seek c env (IfZC env t e : k)
  App _ t u -> seek t env (AppL env u : k)
  Lam _ nm _ (Sc1 t) -> destroy (CFun env nm t) k
  Fix _ f _ x _ (Sc2 t) -> destroy (CFix env f x t) k
  Lit _ l -> destroy (lit2Value l) k
  Let _ n _ t' (Sc1 t) -> seek t' env (LetD env n t : k)
  Var _ (Bound b) -> abort "unimplemented" -- acá qué hay que hacer?
  Var _ (Free nm) -> abort "unimplemented" -- entiendo que acá tendríamos que fallar
  Var _ (Global nm) -> do
    t <- lookupDecl nm
    case t of
      Nothing -> abort "No le encontramos la variable global"
      Just val -> seek val env k -- pero este val tiene tipo term, pero sabemos que es un val, y si cambiamos de modo?

destroy :: (MonadFD4 m) => Value -> Continuation -> m Value
destroy v [] = return v
destroy v (fr : k) = case fr of
  PntT lit -> destroy v k -- Nos falta imprimir?
  BOpL env op term -> seek term env (BOpR op v : k)
  BOpR op value -> case (value, v) of
    (VNat l, VNat r) -> destroy (VNat $ semOp op l r) k
    _ -> abort "error de tipos runtime"
  IfZC env t e -> case v of
    VNat 0 -> seek t env k
    VNat _ -> seek e env k
    _ -> abort "error de tipos runtime"
  AppL env t -> seek t env (AppR v : k)
  AppR value -> case value of
    CFun env x t -> seek t (v : env) k
    CFix env f x t -> seek t (value : v : env) k
    _ -> abort "error de tipos runtime"
  LetD env _ t -> seek t (v : env) k -- olvido tu nombre?

eval :: (MonadFD4 m) => TTerm -> m TTerm
-- Ayer escribimos esto
-- eval t = do
--   v <- seek t [] []
--   return (val2TTerm v)

-- Hoy se me ocurre esto
-- eval t = fmap val2TTerm $ seek t [] []

-- el linter me dice que haga esto
eval t = val2TTerm <$> seek t [] []
