module ClosureConvert where

import Common (abort)
import Control.Monad.State
import Control.Monad.Writer
import Core
import Data.Composition
import IR
import Subst

type M w a = StateT Int (Writer w) a

runM :: Int -> M w a -> ((a, Int), w)
runM i m = runWriter $ runStateT m i

runCC :: Module -> [IrDecl]
runCC = snd . runM 0 . mapM convertDecl

convertDecl :: Decl TTerm -> M [IrDecl] ()
convertDecl d = do
  ir <- convertTerm d.body
  tell . pure $ case ir of
    MkClosure nom verdura -> IrFun d.name (convertTy $ getTy d.body) [] ir
    i@(IrBinaryOp bop ir1 ir2) -> IrVal d.name (convertTy $ getTy d.body) i
    _ -> IrVal d.name (convertTy $ getTy d.body) ir



convertTy :: Ty -> IrTy
convertTy Nat = IrInt
convertTy (Arrow _ _) = IrClo
convertTy String = abort "error"
convertTy (Named _) = abort "error"
convertTy Unit = abort "error"

convertTerm :: TTerm -> M w Ir
convertTerm (Var _ (Bound i)) = return $ case i of _ -> _
convertTerm (Var _ (Free n)) = abort "Free Variable"
convertTerm (Var _ (Global n)) = return $ IrGlobal n
convertTerm (Lit _ c) = return $ IrConst c
convertTerm (BOp _ op t1 t2) = do
  ccT1 <- convertTerm t1
  ccT2 <- convertTerm t2
  return $ IrBinaryOp op ccT1 ccT2
convertTerm (IfZ _ c t e) = do
  ccC <- convertTerm c
  ccT <- convertTerm t
  ccE <- convertTerm e
  return $ IrIfZ ccC ccT ccE
convertTerm (Pnt _ s t) = do
  ccT <- convertTerm t
  return $ IrPrint (show s) ccT
convertTerm (App (_, fty) f x) = do
  ccF <- convertTerm f
  ccX <- convertTerm x
  let clos0 = IrAccess ccF IrClo 0
  -- el IrClo hace referencia al primer elemento de ccf
  -- o hace referencia al ccf[0] ??
  let args = [ccF, ccX]
  let irCall = IrCall clos0 args (convertTy fty)
  return $ IrLet "func" IrClo ccF irCall
convertTerm (Lam (_, fty) n _ t@(Sc1 _)) = do
  fr <- get
  put $ fr + 1
  return $ MkClosure ("__f" ++ show fr) freeNames
  where
    body = open n t
    freeNames = map IrVar (freeVars body)
convertTerm (Let _ xn xty bdy (Sc1 t)) = do
  decl <- convertTerm bdy
  scp <- convertTerm t
  return $ IrLet xn (convertTy xty) decl scp
convertTerm (Fix _ _ _ _ _ _) = _
