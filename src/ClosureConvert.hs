module ClosureConvert where

import Common (abort)
import Control.Monad.State
import Control.Monad.Writer
import Core
import Data.Composition ()
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

freshName :: String -> StateT Int (Writer [IrDecl]) String
freshName prefix = do
    fr <- get
    put (fr+1)
    return (prefix ++ "_" ++  show fr)


convertTy :: Ty -> IrTy
convertTy Nat = IrInt
convertTy (Arrow _ _) = IrClo
convertTy String = abort "error"
convertTy (Named _) = abort "error"
convertTy Unit = abort "error"

convertTerm :: TTerm -> StateT Int (Writer [IrDecl]) Ir
convertTerm (Var _ (Bound i)) = abort "CC -> Bound Variable"
convertTerm (Var _ (Free n)) = return $ IrVar n
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
  fName <- freshName "appFun"
  let clos0 = IrAccess ccF IrClo 0
  -- el IrClo hace referencia al primer elemento de ccf
  -- o hace referencia al ccf[0] ??
  let args = [ccF, ccX]
  let irCall = IrCall clos0 args (convertTy fty)
  return $ IrLet fName IrClo ccF irCall

convertTerm (Let _ xn xty alias sc@(Sc1 bdy)) = do
  xName <- freshName xn
  decl <- convertTerm alias
  scp <- convertTerm (open xName sc)
  return $ IrLet xName (convertTy xty) decl scp

convertTerm (Lam pos xn xty sc@(Sc1 bdy)) = do
  let xTy = convertTy xty
  funName <- freshName $ "fun"
  closName <- freshName $ funName ++ "_closure"
  xName <- freshName xn
  let freeVarsInBody = freeVarsWithTheirType bdy -- ¿sale sólo la x? ¿debo tener algo?
  let openned = open xName sc -- ¿deberíamos usar xName o xn?
  convertedBody <- convertTerm openned
  -- deberíamos reemplazar en convertedBody las variables libres por el valor 
  -- <supongo que con IrAccess>
  -- let replaced = replaceFrees freeVarsInBody convertedBody
  let funDecl = IrFun funName xTy [(closName, IrClo), (xn, IrInt)] convertedBody
  -- tell funDecl
  let freeNames = map IrVar (map fst freeVarsInBody)
  return $ MkClosure closName freeNames


convertTerm (Fix i fn fty xn xty sc@(Sc2 bdy)) = do 
  let funTy = convertTy fty
  let xTy = convertTy xty
  funName <- freshName $ "fun_" ++ show fn
  closName <- freshName $ funName ++ "_closure"
  xName <- freshName xn
  
  let freeVarsInBody = freeVarsWithTheirType bdy -- ¿sale sólo la x? ¿debo tener algo?
  
  let openned = open2 fn xn sc
  convertedBody <- convertTerm openned

  -- deberíamos reemplazar en convertedBody las variables libres por el valor 
  -- <supongo que con IrAccess>
  -- let replaced = replaceFrees freeVarsInBody convertedBody

  let funDecl = IrFun funName funTy [(closName, IrClo), (xn, xTy)] convertedBody
  -- tell funDecl
  let freeNames = map IrVar (map fst freeVarsInBody)
  return $ MkClosure funName freeNames
