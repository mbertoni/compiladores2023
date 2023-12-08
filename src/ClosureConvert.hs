module ClosureConvert where

import Common (abort)
import Control.Monad.State
import Control.Monad.Writer
import Core
import GHC.IO.Handle.Types (Handle__)
import IR
import Subst

convertType :: Ty -> IrTy
-- TODO
convertType Nat = IrInt
convertType (Arrow _ _) = IrClo
convertType String = abort "error"
convertType (Named _) = abort "error"
convertType Unit = abort "error"

type Ms = StateT Int (Writer [IrDecl])

closureConvert :: TTerm -> Ms Ir
closureConvert (Var _ (Bound i)) = return $ case i of IrVar (fresh )
closureConvert (Var _ (Free n)) = abort "Free Variable"
closureConvert (Var _ (Global n)) = return $ IrGlobal n
closureConvert (Lit _ c) = return $ IrConst c
closureConvert (BOp _ op t1 t2) = do
  ccT1 <- closureConvert t1
  ccT2 <- closureConvert t2
  return $ IrBinaryOp op ccT1 ccT2
closureConvert (IfZ _ c t e) = do
  ccC <- closureConvert c
  ccT <- closureConvert t
  ccE <- closureConvert e
  return $ IrIfZ ccC ccT ccE
closureConvert (Pnt _ s t) = do
  ccT <- closureConvert t
  return $ IrPrint (show s) ccT
closureConvert (App (_, fty) f x) = do
  ccF <- closureConvert f
  ccX <- closureConvert x
  let clos0 = IrAccess ccF IrClo 0
  -- el IrClo hace referencia al primer elemento de ccf
  -- o hace referencia al ccf[0] ??
  let args = [ccF, ccX]
  let irCall = IrCall clos0 args (convertType fty)
  return $ IrLet "func" IrClo ccF irCall
closureConvert (Lam (_, fty) n _ t@(Sc1 _)) = do
  fr <- get
  put $ fr + 1
  return $ MkClosure ("__f" ++ show fr) freeNames
  where
    body = open n t
    freeNames = map IrVar (freeVars body)
closureConvert (Let _ xn xty bdy (Sc1 t)) = do
  decl <- closureConvert bdy
  scp <- closureConvert t
  return $ IrLet xn (convertType xty) decl scp
closureConvert t = abort "Unimplemented PM"

-- Resta App, Fix

runCC' :: Decl TTerm -> Ms ()
runCC' d = do
  ir <- closureConvert d.body
  tell . pure $ case ir of
    MkClosure nom verdura -> IrFun d.name (convertType $ getTy d.body) [] ir
    _ -> IrVal d.name (convertType $ getTy d.body) ir

runCC :: Module -> [IrDecl]
runCC m = runner $ mapM_ runCC' m

runner :: Ms a -> [IrDecl]
runner m = snd . runWriter $ runStateT m 0
