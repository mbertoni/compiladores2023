

data Var = N Nat | C Closure

data Closure    = ClosureFun Info Environment Name Ty TTerm 
                | ClosureFix Info Environment Name Ty Var TTerm

type Environment = [Var]

data Frame  = KArg Environment TTerm
            | KClos Clos
            | KIfz Env TTerm TTerm 
            | KBin Env TTerm
            | KVar
            | KPrint String 
            | KLet Env Name TTerm

type Kont = [Frame]

seek :: MonadFD4 m => TTerm -> Env -> Kont -> m Val -- <>
-- < print s t , ρ, k> -> <t , ρ, (print s □):k >
seek (Print _ n t) env k = seek t env ( (KPrint n):k) -- La info se pierde _

seek (IfZ i c t t') env k = seek c env ((KIfz env t t'):k)

seek (App i t t') env k = seek t env ((KArg env t'):k)

destroy :: MonadFD4 m => Val -> Kont -> m Val       -- <<>>