module CEK where

import Lang
import MonadFD4
import Common 

data Val = VConst Const | VClosure Closure

data Closure =
    ClosureFun Env Name TTerm
    | ClosureFix Env Name Name TTerm

type Env = [Val] -- rho

type Kont = [Frame]
data Frame  = KArgL Env TTerm   -- (□ u)
            | KArgR Closure     -- (t □)
            | KClos Closure
            | KIfz Env TTerm TTerm
            | KBinL Env BinaryOp TTerm -- (□ + u)
            | KBinR Env BinaryOp TTerm -- (t + □)
            | KVar Var
            | KPrint String
            | KLet Env Name TTerm


seek :: MonadFD4 m => TTerm -> Env -> Kont -> m Val -- <>
-- < print s t , ρ, k> -> <t , ρ, (print s □):k >
seek (Print _ s t) env k        = seek t env ((KPrint s):k) -- La info se pierde, supongo
seek (BinaryOp i op t u) env k  = seek t env ((KBinL env op u):k)
seek (IfZ i c t e) env k        = seek c env ((KIfz env t e):k)
seek (App i t u) env k          = seek t env ((KArgL env u):k)
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
seek _ env k = abort "hacer el ejercicio de la practica"
-- resta hacer el let

destroy :: MonadFD4 m => Val -> Kont -> m Val       -- <<>>
destroy v [] = return v
destroy v ((KPrint str):k)          = destroy v k -- Nos falta imprimir?
destroy n ((KBinL env op u):k)      = seek u env ((KBinR env op n):k)
destroy n' ((KBinR env op n):k)     = destroy (N op n n') k
destroy (VConst 0) ((KIfz env t e):k)    = seek t env k
destroy (VConst _) ((KIfz env t e):k)    = seek e env k
destroy (KClos clos) ((KArgL env t):k)                  = seek t env ((KClos clos):k)
destroy v ((KArgR (ClosureFun env x t)):k)            = seek t (v:env) k
destroy v ((KArgR clos@(ClosureFix env f x t) ):k)    = seek t ((Cnv clos):v:e) k
-- resta hacer el let

{-
eval :: MonadFD4 m => TTerm -> m TTerm
eval t = do v <- seek t [] []
            return (valToTTerm v)
-}