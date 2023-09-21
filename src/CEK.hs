

data Val = N Nat | C Closure

data Closure    = ClosureFun Info Env Name TTerm 
                | ClosureFix Info Env Name Val TTerm

type Env = [Val]

data Frame  = KArgL Env TTerm   -- (□ u)
            | KArgR Closure     -- (t □)
            | KClos C
            | KIfz Env TTerm TTerm 
            | KBinL Env TTerm -- (□ + u)
            | KBinR Env TTerm -- (t + □)
            | KVar Var
            | KPrint String 
            | KLet Env Name TTerm

type Kont = [Frame]

seek :: MonadFD4 m => TTerm -> Env -> Kont -> m Val -- <>
-- < print s t , ρ, k> -> <t , ρ, (print s □):k >
seek (Print _ s t) env k        = seek t env ((KPrint s):k) -- La info se pierde, supongo
seek (BinaryOp i op t u) env k  = seek t env ((KBinL env op u):k) 
seek (IfZ i c t e) env k        = seek c env ((KIfz env t e):k)
seek (App i t u) env k          = seek t env ((KArgL env u):k)
seek (V i v Bound b) env k      = abort "unimplemented" -- acá qué hay que hacer?
seek (V i v Free nm) env k      = abort "unimplemented" -- entiendo que acá tendríamos que fallar       
seek (V i v Global nm) env k    = do value <- lookupDecl nm case value of 
                                    Nothing -> abort "No encontramos el la variable global"
                                    (Just v) -> destroy v k
seek (Lam i nm _ (Sc1 t _ _)) env k     = destroy (KClos C ClosureFun i env nm t) k -- ver si hay que sacar ty
seek (Fix i f _ x _ (Sc2 t _ _)) env k  = destroy (KClos C ClosureFix i env f x t) k 
-- resta hacer el let

destroy :: MonadFD4 m => Val -> Kont -> m Val       -- <<>>
destroy v [] = return v
destroy v ((KPrint str):k)          = destroy v k -- Nos falta imprimir?
destroy n ((KBinL env op u):k)      = seek u env ((KBinR env op n):k) 
destroy n' ((KBinR env op n):k)     = destroy (N op n n') k
destroy (N 0) ((KIfz env t e):k)    = seek t env k
destroy (N _) ((KIfz env t e):k)    = seek e env k
destroy (KClos clos) ((KArgL env t):k)                  = seek t env ((KClos clos):k)
destroy v ((KArgR (ClosureFun i env x t)):k)            = seek t (v:env) k
destroy v ((KArgR clos@(ClosureFix i env f x t) ):k)    = seek t ((C clos):v:env) k
-- resta hacer el let

{-
eval :: MonadFD4 m => TTerm -> m TTerm
eval t = do v <- seek t [] []
            return (valToTTerm v) 
-}