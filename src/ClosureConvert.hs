import Core
import IR
import Common (abort)
import Control.Monad.State
import Control.Monad.Writer
import GHC.Stack (ccsToStrings)

convertType :: Ty -> IrTy
convertType Nat = IrInt
convertType Arrow = IrClo
convertType String = abort "error"
convertType Unit = abort "error"


closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert (Var _ (Bound i)) = return
closureConvert (Var _ (Free n)) = return $ IrVar n
closureConvert (Var _ (Global n)) = return $ IrGlobal n
closureConvert (Lit _ c) = return $ IrConst c
closureConvert (Lam (fn,fty) n _ s) = do    fresh <-  get
                                            put $ fresh + 1
                                            frName <- "__f" ++ show fresh
                                            IrFun frName (convertType fty) 

closureConvert (BOp _ op t1 t2) = return $ IrBinaryOp op (closureConvert t1) (closureConvert t2)
closureConvert (IfZ _ c t e) = do   ccC <- closureConvert c
                                    ccT <- closureConvert t
                                    ccE <- closureConvert e
                                    return $ IrIfZ ccC ccT ccE
closureConvert (Pnt _ s t) = return $ IrPrint s t


