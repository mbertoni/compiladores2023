module CEK (eval) where

import Common
import Core
import Eval (semOp)
import MonadFD4

data Value = Const Literal | Clos Closure
  deriving (Show)

val2TTerm :: Value -> TTerm
val2TTerm (Const c) = Lit (NoPos, Nat) c
val2TTerm (Clos f) = abort "rough operator"

data Closure
  = ClosFun Env Name TTerm
  | ClosFix Env Name Name TTerm
  deriving (Show)

type Env = [Value]

type Continuation = [Frame]

data Frame
  = AppL Env TTerm -- (App □ arg)
  | AppR Closure -- (App f □)
  | IfZC Env TTerm TTerm -- (IfZ □ then else)
  | BOpL Env BinaryOp TTerm -- (□ (+) u)
  | BOpR BinaryOp Value -- (v (+) □)
  | VarT Var
  | PntT Literal -- (print s □)
  | LetD Env Name TTerm -- let □ in term
  deriving (Show)

seek :: (MonadFD4 m) => TTerm -> Env -> Continuation -> m Value
seek term env k = case term of
  Pnt _ s t -> seek t env (PntT s : k)
  BOp _ op t u -> seek t env (BOpL env op u : k)
  IfZ _ c t e -> seek c env (IfZC env t e : k)
  App _ t u -> seek t env (AppL env u : k)
  Lam _ nm _ (Sc1 t) -> destroy (Clos (ClosFun env nm t)) k
  Fix _ f _ x _ (Sc2 t) -> destroy (Clos (ClosFix env f x t)) k
  Lit _ c -> destroy (Const c) k
  Let _ n _ def (Sc1 t) -> seek def env (LetD env n t : k)
  Var _ (Bound b) -> abort "unimplemented" -- acá qué hay que hacer?
  Var _ (Free nm) -> abort "unimplemented" -- entiendo que acá tendríamos que fallar
  Var _ (Global nm) -> do
    t <- lookupDecl nm
    case t of
      Nothing -> abort "No le encontramos la variable global"
      Just val -> seek val env k -- pero este val tiene tipo term, pero sabemos que es un val, y si cambiamos de modo?

destroy :: (MonadFD4 m) => Value -> Continuation -> m Value
destroy v [] = return v
destroy v (PntT str : k) = destroy v k -- Nos falta imprimir?
destroy v (BOpL env op rt : k) = seek rt env (BOpR op v : k)
destroy v (BOpR op lv : k) = case (lv, v) of
  (Const (N l), Const (N r)) -> destroy (Const (N (semOp op l r))) k
  _ -> abort "error de tipos runtime"
destroy (Const (N 0)) (IfZC env t e : k) = seek t env k
destroy (Const (N _)) (IfZC env t e : k) = seek e env k
destroy (Clos clos) (AppL env t : k) = seek t env (AppR clos : k)
destroy v (AppR (ClosFun env x t) : k) = seek t (v : env) k
destroy v (AppR clos@(ClosFix env f x t) : k) = seek t (Clos clos : v : env) k
destroy v (LetD env _ t : k) = seek t (v : env) k -- olvido tu nombre?
destroy v _ = abort "its no possible Blenda"

eval :: (MonadFD4 m) => TTerm -> m TTerm
-- Ayer escribimos esto
-- eval t = do
--   v <- seek t [] []
--   return (val2TTerm v)

-- Hoy se me ocurre esto
-- eval t = fmap val2TTerm $ seek t [] []

-- el linter me dice que haga esto
eval t = val2TTerm <$> seek t [] []
