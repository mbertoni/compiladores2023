module Optimizer where

import Core
import MonadFD4


optimize :: MonadFD4 m => TTerm -> m TTerm
optimize t = go t optimizeQty where
    go t 0 = return t
    go t n = do go t (n-1)
    optimizeQty = 10

constantFolding :: MonadFD4 m => TTerm -> m TTerm
constantFolding v@(Var _ _) = return v
-- ver el resto de los casos
constantFolding t = return t 

constantPropagation :: MonadFD4 m => TTerm -> m TTerm
constantPropagation v@(Var _ _) = return v
constantPropagation (Lam i n ty (Sc1 t)) = do
    t' <- constantPropagation t
    return $ Lam i n ty (Sc1 t')
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
constantPropagation (Let i x xty def (Sc1 body)) = do
    def' <- constantPropagation def
    return def'
    -- hay q expandir esto. Es el caso base
constantPropagation t = return t 