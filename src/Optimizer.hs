module Optimizer where

import Core
import MonadFD4
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

-- visit :: (TTerm -> TTerm) -> TTerm -> TTerm
visit g v@(Var _ _) = v
visit g l@(Lit _ _) = l
visit g (Lam i n ty (Sc1 t)) = Lam i n ty (Sc1 (visit g t))
visit g (App i t1 t2) = App i (visit g t1) (visit g t2)
visit g (Pnt i l t) = Pnt i l (visit g t)
visit g (BOp i op t1 t2) = BOp i op (visit g t1) (visit g t2)
visit g (Fix i f fty x xty (Sc2 t)) = Fix i f fty x xty (Sc2 (visit g t))
visit g (IfZ i c t e) = IfZ i (visit g c) (visit g t) (visit g e)
visit g (Let i x xty alias (Sc1 body)) = Let i x xty (visit g alias) (Sc1 (visit g body))

constantFolding :: TTerm -> TTerm
constantFolding = visit go
    where   go (IfZ i c t e) = case c' of 
                Lit _ (N 0) -> go t
                Lit _ (N _) -> go e
                term -> IfZ i c' t' e
                where
                    c' = visit go c -- no estoy seguro si es visit go o solamente go
                    t' = visit go t
                    e' = visit go e
            go (BOp i op t1 t2) =  case t2' of 
                -- Tendríamos que ver el print acá, ¿no?
                    -- Si t2 es 0, retorno t1
                    Lit _ (N 0) -> t1' 
                    _ -> case t1' of    Lit _ (N 0) -> 
                                            case op of  Add -> t2'
                                                        Sub -> Lit i (N 0)
                                        _ -> BOp i op t1' t2' 
                    where   t1' = visit go t1
                            t2' = visit go t2
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

------------------------------------------------------------
------------------------------------------------------------
-- constantFolding :: MonadFD4 m => TTerm -> m TTerm
-- constantFolding v@(Var _ _) = return v
-- constantFolding l@(Lit _ _) = return l
-- constantFolding (Lam i n ty (Sc1 t)) = do
--     t' <- constantFolding t
--     return $ Lam i n ty (Sc1 t')
-- constantFolding (App i t1 t2) = do
--     t1' <- constantFolding t1
--     t2' <- constantFolding t2
--     return $ App i t1' t2'
-- constantFolding (Pnt i st t) = do
--     t' <- constantFolding t
--     return $ Pnt i st t'
-- constantFolding (Fix i f fty x xty (Sc2 t)) = do
--     t' <- constantFolding t
--     return $ Fix i f fty x xty (Sc2 t')
-- constantFolding (Let i x xty alias (Sc1 body)) = do
--     alias' <- constantFolding alias
--     body' <- constantFolding body
--     return $ Let i x xty alias' (Sc1 body')
-- constantFolding (IfZ i c t e) = do
--     c' <- constantFolding c
--     case c' of 
--         Lit _ (N 0) -> constantFolding t
--         Lit _ (N _) -> constantFolding e
--         term -> do
--             t' <- constantFolding t
--             e' <- constantFolding e
--             return $ IfZ i c' t' e' 
-- constantFolding (BOp i op t1 t2) = do 
-- -- Tendríamos que ver el print acá, ¿no?
--     t1' <- constantFolding t1
--     t2' <- constantFolding t2
--     case t2' of
--         -- Si t2 es 0, retorno t1
--         Lit _ (N 0) -> return t1' 
--         _ -> case t1' of  Lit _ (N 0) -> case op of  
--                             Add -> return t2'
--                             Sub -> return $ Lit i (N 0)
--                           _ -> return $ BOp i op t1' t2' 
------------------------------------------------------------
------------------------------------------------------------
-- constantPropagation :: MonadFD4 m => TTerm -> m TTerm
-- constantPropagation v@(Var _ _) = return v
-- constantPropagation (Lam i l ty (Sc1 t)) = do
--     t' <- constantPropagation t
--     return $ Lam i l ty (Sc1 t')
-- constantPropagation (App i t1 t2) = do
--     t1' <- constantPropagation t1
--     t2' <- constantPropagation t2
--     return $ App i t1' t2'
-- constantPropagation (Pnt i st t) = do
--     t' <- constantPropagation t
--     return $ Pnt i st t'
-- constantPropagation (Fix i f fty x xty (Sc2 t)) = do
--     t' <- constantPropagation t
--     return $ Fix i f fty x xty (Sc2 t')
-- constantPropagation (IfZ i c t e) = do
--     c' <- constantPropagation c
--     t' <- constantPropagation t
--     e' <- constantPropagation e
--     return $ IfZ i c' t' e' 
-- constantPropagation (BOp i op t1 t2) = do 
--     t1' <- constantPropagation t1
--     t2' <- constantPropagation t2
--     return $ BOp i op t1' t2'
-- constantPropagation p@(Pnt i l t) = return p 
    -- abort "Chequear el tema de los prints" 
-- constantPropagation (Let i x xty alias (Sc1 body)) = do
--     alias' <- constantPropagation alias
--     case alias' of
--         Lit i2 l -> do  substBody <- constantPropagation body'
--                         return $ Let i x xty alias' (Sc1 substBody)
--                         where body' = subst (Lit i2 l) (Sc1 body)
--         t -> do
--             substBody <- constantPropagation body
--             return $ Let i x xty alias' (Sc1 substBody)
-- constantPropagation t = return t 
------------------------------------------------------------
------------------------------------------------------------