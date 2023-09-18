{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@STerm) a locally closed (@Term@)
-}

module Elab ( elabTerm, elabDeclaration) where

import Common (abort)
import Lang
import Subst


-- | 'go transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elabTerm :: _types -> STerm -> Term
elabTerm types = go [] where
  go' :: STy -> Ty
  go' = elabType types

  go :: [Name] -> STerm -> Term
  go env (SV p v) =
    -- Tenemos que ver si la variable es Global o es un nombre local
    -- En env llevamos la lista de nombres locales.
    if v `elem` env 
      then V p (Free v)
      else V p (Global v)

  go _ (SConst p c) = Const p c
  go env (SLam p [] t) = abort "Empty lambda binding list"
  go env (SLam p [(v,ty)] t) = Lam p v (go' ty) (close v (go (v:env) t))
  go env (SLam p ((v,ty):bs) t) = Lam p v (go' ty) (close v (go (v:env) (SLam p bs t) ))
  go env (SFix i (f,fty) [] t) = abort "Empty fix binding list"
  go env (SFix i (f,fty) [(x,xty)] t) = Fix i f (go' fty) x (go' xty) (close2 f x (go (x:f:env) t))
  go env (SFix i (f,fty) ((x,xty):bs) t) = 
    Fix i f (go' fty) x (go' xty) (close2 f x (go (x:f:env) (SLam i bs t)))
  go env (SIfZ p c t e)         = IfZ p (go env c) (go env t) (go env e)
  -- des hardcodear el Bang
  go env (SLetFun i (fn,ty) bs t t' ) = 
    go env (SLet i (fn, funTy) (SLam i bs t) t')
      where funTy = sTyFold (map snd bs ++ [ty])  
    
  go env (SUnaryOp i Bang t) = IfZ i (go env t) (Const i (CNat 1)) (Const i (CNat 0))
  go env (SIf i _) = abort "unimplemented" 
  -- Operadores binarios
  go env (SBinaryOp i o t u) = BinaryOp i o (go env t) (go env u)
  -- Operador Print
  go env (SPrint i str t) = Print i str (go env t)
  -- Aplicaciones generales
  go env (SApp p h a) = App p (go env h) (go env a)
  go env (SLet p (v,vty) def body) =  
    Let p v (go' vty) (go env def) (close v (go (v:env) body))
  go env (SLetRec i (f,ty) [] t t') = abort "Empty let rec list"
  go env (SLetRec i (f,ty) (b:bs) t t') = 
    go env (SLet i (f, funTy) (SFix i (f, funTy) (b:bs) t) t')
      where funTy = sTyFold (map snd (b:bs) ++ [ty])  

elabType :: _types -> STy -> Ty
elabType types = _

elabDeclaration :: _types -> SDeclaration -> Decl (Either Term Ty)
elabDeclaration types decl = Decl {declName = decl.sDeclName, declPos = decl.sDeclPos, declBody = body} where
  body = case decl.sDeclBody of
    STermDecl sTerm -> Left $ elabTerm types sTerm
    STypeDecl sType -> Right $ elabType types sType



