module Optimizer where

import Core
import MonadFD4
import Subst


optimize :: MonadFD4 m => TTerm -> m TTerm
optimize t = go optimizeQty t where
    go:: MonadFD4 m => Int -> TTerm -> m TTerm 
    go 0 tt = return tt
    go n tt = do 
        t1 <- constantFolding tt
        t2 <- constantPropagation t1
        t3 <- go (n-1) t2
        return $ t3
    optimizeQty = 10


constantFolding :: MonadFD4 m => TTerm -> m TTerm
constantFolding v@(Var _ _) = return v
constantFolding (Lam i n ty (Sc1 t)) = do
    t' <- constantFolding t
    return $ Lam i n ty (Sc1 t')
constantFolding (App i t1 t2) = do
    t1' <- constantFolding t1
    t2' <- constantFolding t2
    return $ App i t1' t2'
constantFolding (Pnt i st t) = do
    t' <- constantFolding t
    return $ Pnt i st t'
constantFolding (Fix i f fty x xty (Sc2 t)) = do
    t' <- constantFolding t
    return $ Fix i f fty x xty (Sc2 t')
-- ver el resto de los casos
constantFolding t = return t 

constantPropagation :: MonadFD4 m => TTerm -> m TTerm
constantPropagation v@(Var _ _) = return v
constantPropagation (Lam i l ty (Sc1 t)) = do
    t' <- constantPropagation t
    return $ Lam i l ty (Sc1 t')
constantPropagation (App i t1 t2) = do
    t1' <- constantPropagation t1
    t2' <- constantPropagation t2
    return $ App i t1' t2'
constantPropagation (Pnt i st t) = do
    t' <- constantPropagation t
    return $ Pnt i st t'
constantPropagation (Fix i f fty x xty (Sc2 t)) = do
    t' <- constantPropagation t
    return $ Fix i f fty x xty (Sc2 t')
constantPropagation (IfZ i c t0 t1) = do
    c' <- constantPropagation c
    t0' <- constantPropagation t0
    t1' <- constantPropagation t1
    return $ IfZ i c' t0' t1' 
constantPropagation (BOp i op t1 t2) = do 
    t1' <- constantPropagation t1
    t2' <- constantPropagation t2
    return $ BOp i op t1' t2'
constantPropagation p@(Pnt i l t) = return p -- abort "Chequear el tema de los prints" 
constantPropagation (Let i x xty alias (Sc1 body)) = do
    alias' <- constantPropagation alias
    case alias' of
        Lit i2 l -> do  substBody <- constantPropagation body'
                        return $ Let i x xty alias' (Sc1 substBody)
                        where body' = subst (Lit i2 l) (Sc1 body)
        t -> do
            substBody <- constantPropagation body
            return $ Let i x xty alias' (Sc1 substBody)
constantPropagation t = return t 