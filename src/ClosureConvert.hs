module ClosureConvert where

import Core
import IR
import Common (abort)
import Subst
import Control.Monad.State
import Control.Monad.Writer

convertType :: Ty -> IrTy
-- TODO
convertType Nat = IrInt
convertType (Arrow _ _) = IrClo
convertType String = abort "error"
convertType (Named _) = abort "error"
convertType Unit = abort "error"

type Ms a = StateT Int (Writer [IrDecl]) a
-- newtype IrDecls [IrDecl]

closureConvert :: TTerm -> Ms Ir
closureConvert (Var _ (Bound i))  = abort "Unimplemented" 
-- No estoy seguro qué deberíamos devolver, ¿tenemos que usar IrAccess?
closureConvert (Var _ (Free  n))  = return $ IrVar n
closureConvert (Var _ (Global n)) = return $ IrGlobal n
closureConvert (Lit _ c) = return $ IrConst c

closureConvert (BOp _ op t1 t2) = do    ccT1 <- closureConvert t1
                                        ccT2 <- closureConvert t2
                                        return $ IrBinaryOp op ccT1 ccT2
closureConvert (IfZ _ c t e) = do   ccC <- closureConvert c
                                    ccT <- closureConvert t
                                    ccE <- closureConvert e
                                    return $ IrIfZ ccC ccT ccE
closureConvert (Pnt _ s t) = do ccT <- closureConvert t
                                return $ IrPrint (show s) ccT

closureConvert (App (_,fty) f x) = do   ccF <- closureConvert f
                                        ccX <- closureConvert x
                                        let clos0 = IrAccess ccF IrClo 0
                            -- el IrClo hace referencia al primer elemento de ccf
                            -- o hace referencia al ccf[0] ??
                                        let args = [ccF,ccX]
                                        let irCall = IrCall clos0 args (convertType fty)
                                        return $ IrLet "func" IrClo ccF irCall                                                  
                                      

closureConvert (Lam (_,fty) n _ t@(Sc1 _)) = do fr <- get
                                                put $ fr + 1                                                
                                                return $ MkClosure ("__f" ++ show fr) freeNames
                                                where   body = open n t
                                                        freeNames = map IrVar (freeVars body)
                                                        
closureConvert (Let _ xn xty bdy (Sc1 t)) = do  decl <- closureConvert bdy
                                                scp <- closureConvert t 
                                                return $ IrLet xn (convertType xty) decl scp
closureConvert t = abort "Unimplemented PM"
-- Resta App, Fix

runCC' :: Module -> Ms [IrDecl]
runCC' [] = return []
runCC' (d:ds) = do
    tt <- closureConvert d.body
    dst <- runCC' ds
    return $ IrVal d.name (convertType $ getTy d.body) tt:dst
    
runCC:: [Decl Term] -> IrDecls
runCC = abort "Unimplemented"