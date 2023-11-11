import Core
import IR
import Common (abort)
import Subst
import Control.Monad.State
import Control.Monad.Writer

convertType :: Ty -> IrTy
convertType Nat = IrInt
convertType (Arrow _ _) = IrClo
convertType String = abort "error"
convertType (Named _) = abort "error"
convertType Unit = abort "error"


closureConvert :: TTerm -> StateT Int (Writer [IrDecl]) Ir
closureConvert (Var _ (Bound i)) = abort "Unimplemented" -- No estoy seguro qué deberíamos devolver
closureConvert (Var _ (Free n)) = return $ IrVar n
closureConvert (Var _ (Global n)) = return $ IrGlobal n
closureConvert (Lit _ c) = return $ IrConst c
closureConvert (Lam (_,fty) n _ t@(Sc1 _)) = do fr <- get
                                                put $ fr + 1                                                
                                                body <- closureConvert (open n t)
                                                return $ MkClosure ("__f" ++ show fr) []

closureConvert (BOp _ op t1 t2) = do    ccT1 <- closureConvert t1
                                        ccT2 <- closureConvert t2
                                        return $ IrBinaryOp op ccT1 ccT2
closureConvert (IfZ _ c t e) = do   ccC <- closureConvert c
                                    ccT <- closureConvert t
                                    ccE <- closureConvert e
                                    return $ IrIfZ ccC ccT ccE
closureConvert (Pnt _ s t) = do ccT <- closureConvert t
                                return $ IrPrint (show s) ccT

closureConvert (Let _ xn xty bdy (Sc1 t)) = do  decl <- closureConvert bdy
                                                scp <- closureConvert t 
                                                return $ IrLet xn (convertType xty) decl scp
closureConvert t = abort "Unimplemented PM"
-- closureConvert (App _ t1 t2) = do   ccT1 <- closureConvert t1
--                                     ccT2 <- closureConvert t2
--                                     return $ 
-- Resta App, Fix

runCC' :: Module -> StateT Int (Writer [IrDecl]) [IrDecl]
runCC' [] = return []
runCC' (d:ds) = do
    tt <- closureConvert dbody
    dst <- runCC' ds
    return $ (IrVal dname (convertType $ getTy dbody) tt):dst
    where   dbody = body d
            dname = name d

