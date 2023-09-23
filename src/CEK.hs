module CEK where

import Lang
import MonadFD4
import Common
import Eval (semOp)

data Val = VConst Const | VClosure Closure
    deriving Show

val2TTerm :: Val -> TTerm
val2TTerm (VConst c) = Const (NoPos, NatTy) c
val2TTerm (VClosure f) = abort "por ahora no tenemos operadores de alto orden, Seba hacete el apply y el comp"

data Closure =
    ClosureFun Env Name TTerm
    | ClosureFix Env Name Name TTerm
    deriving Show

type Env = [Val] -- rho

type Kont = [Frame]
data Frame  = KArgL Env TTerm          -- (App □ arg)
            | KArgR Closure            -- (App f □)
            | KIfz Env TTerm TTerm     -- (IfZ □ then else)
            | KBinL Env BinaryOp TTerm -- (□ (+) u)
            | KBinR BinaryOp Val       -- (v (+) □)
            | KVar Var
            | KPrint String            -- (print s □)
            | KLetDef Env Name TTerm   -- let □ in term
            -- | KLetBody Env Name TTerm  -- let def in □
            deriving Show

seek :: MonadFD4 m => TTerm -> Env -> Kont -> m Val
seek (Print _ s t) env k = seek t env (KPrint s:k) -- < print s t , ρ, k> -> <t , ρ, (print s □):k >
seek (BinaryOp i op t u) env k  = seek t env (KBinL env op u : k)
seek (IfZ i c t e) env k        = seek c env (KIfz env t e : k)
seek (App i t u) env k          = seek t env (KArgL env u : k)
seek (V i (Bound b)) env k      = abort "unimplemented" -- acá qué hay que hacer?
seek (V i (Free nm)) env k      = abort "unimplemented" -- entiendo que acá tendríamos que fallar
seek (V i (Global nm)) env k    = do
    t <- lookupDecl nm
    case t of
        Nothing -> abort "No le encontramos la variable global"
        Just val -> seek val env k -- destroy v k
seek (Lam i nm _ (Sc1 t)) env k     = destroy (VClosure (ClosureFun env nm t)) k
seek (Fix i f _ x _ (Sc2 t)) env k  = destroy (VClosure (ClosureFix env f x t)) k
seek (Const i c) env k = destroy (VConst c) k
seek (Let i n _ def (Sc1 t)) env k = seek def env (KLetDef env n t : k)
-- seek _ env k = abort "hacer el ejercicio de la practica"
-- resta hacer el let

destroy :: MonadFD4 m => Val -> Kont -> m Val       -- <<>>
destroy v [] = return v
destroy v (KPrint str : k)                                 = destroy v k -- Nos falta imprimir?
destroy v (KBinL env op rt : k)                             = seek rt env (KBinR op v :k)
destroy v (KBinR op lv : k)   = case (lv, v) of
    (VConst (CNat l), VConst (CNat r)) -> destroy (VConst (CNat (semOp op l r))) k
    _ -> abort "error de tipos runtime"
destroy (VConst (CNat 0)) (KIfz env t e : k)               = seek t env k
destroy (VConst (CNat _)) (KIfz env t e : k)               = seek e env k
destroy (VClosure clos) (KArgL env t : k)                  = seek t env (KArgR clos : k)
destroy v (KArgR (ClosureFun env x t) : k)                 = seek t (v : env) k
destroy v (KArgR clos@(ClosureFix env f x t) : k)          = seek t (VClosure clos : v : env) k
destroy v (KLetDef env nm body : k)                        = seek body (v : env) k
-- destroy v (KLetBody)                                       = _
destroy v _ = abort "its no possible Blenda"


evalCEK :: MonadFD4 m => TTerm -> m TTerm
evalCEK t = do
    v <- seek t [] []
    return (val2TTerm v)
