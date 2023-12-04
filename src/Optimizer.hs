module Optimizer where

import Core
import Subst


optimize :: TTerm -> TTerm
optimize = go fuel where
    fuel = 10
    go:: Int -> TTerm -> TTerm 
    go 0 tt = tt
    go n tt = t3 
        where   t1 = constantFolding tt
                t2 = constantPropagation t1
                t3 = go (n-1) t2

constantFolding :: TTerm -> TTerm
constantFolding = visit go
    where   go (IfZ i c t e) = case c' of 
                Lit _ (N 0) -> go t
                Lit _ (N _) -> go e
                term -> IfZ i c' t' e
                where
                    c' = constantFolding c -- no estoy seguro si es visit go o solamente go
                    t' = constantFolding t
                    e' = constantFolding e
            go (BOp i op t1 t2) =  case t2' of 
                -- Tendríamos que ver el print acá, ¿no?
                    -- Si t2 es 0, retorno t1
                    Lit _ (N 0) -> t1' 
                    _ -> case t1' of    Lit _ (N 0) -> 
                                            case op of  Add -> t2'
                                                        Sub -> Lit i (N 0)
                                        _ -> BOp i op t1' t2' 
                    where   t1' = constantFolding t1
                            t2' = constantFolding t2
            go term = visit go term    

constantPropagation :: TTerm -> TTerm
constantPropagation = visit go
    where   go (Let i x xty alias (Sc1 body)) = case alias' of
                Lit i2 l -> Let i x xty alias' (Sc1 substBody)
                                where   body' = subst (Lit i2 l) (Sc1 body)
                                        substBody = visit go body'
                term     -> Let i x xty alias' (Sc1 substBody)
                                where substBody = visit go body
                where alias' = visit go alias
            go term = visit go term
