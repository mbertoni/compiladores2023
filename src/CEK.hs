module CEK (eval) where

import Common
import Core
import Data.Default (def)
import Eval (semOp)
import MonadFD4
import Global
import Subst

data Value
  = VNat Int
  | CFun Name TTerm Env
  | CFix Name Name TTerm Env

instance Show Value where
  show (VNat n) = show n
  show x@CFun{} = "Fun: "++ show x
  show x@CFix{} = "Fix: "++ show x

lit2Value :: Literal -> Value
lit2Value (N i) = VNat i
lit2Value (S s) = abort "not implemented"
lit2Value (U _) = abort "not implemented"

val2TTerm :: Value -> TTerm
val2TTerm (VNat i) = Lit (def, Nat) (N i)
val2TTerm (CFun x t e) = substAll termEnv $ Lam i x (Arrow ty tty) (Sc1 t)                          
                          where i@(pos,ty) = getInfo t
                                tty = getTy t
                                termEnv = map val2TTerm e  
val2TTerm (CFix fn xn t e) =  substAll termEnv $ Fix i fn fty xn xty (Sc2 t)
                                where i@(pos,xty) = getInfo t
                                      termEnv = map val2TTerm e  
                                      fty = getTy t -- creo que acá estoy batting the fruit



type Env = [Value]

type Continuation = [Frame]

data Frame
  = AppL TTerm Env -- (App □ arg)
  | AppR Value -- (App f □)
  | IfZC TTerm TTerm Env -- (IfZ □ then else)
  | BOpL BinaryOp TTerm Env -- (□ (+) u)
  | BOpR BinaryOp Value -- (v (+) □)
  | PntT Literal -- (print s □)
  | LetD Name TTerm Env -- let □ in term
  deriving (Show)

seek :: (MonadFD4 m) => TTerm -> Env -> Continuation -> m Value
seek term env k = do  
                      -- printSeekStatus term env k 
                      case term of
                        Pnt _ s t -> seek t env (PntT s : k)
                        BOp _ op t u -> seek t env (BOpL op u env : k)
                        IfZ _ c t e -> seek c env (IfZC t e env : k)
                        App _ t u -> seek t env (AppL u env : k)
                        Lam _ nm _ (Sc1 t) -> destroy (CFun nm t env) k
                        Fix _ f _ x _ (Sc2 t) -> destroy (CFix f x t env) k
                        Lit _ l -> destroy (lit2Value l) k
                        Let _ n _ t' (Sc1 t) -> seek t' env (LetD n t env : k)
                        Var _ (Bound b) -> destroy (env !! b) k
                        Var _ (Free nm) ->
                          -- pueden venir Free en los Scope?
                          abort "No debería haber variables libres" -- entiendo que acá tendríamos que fallar.
                        Var _ (Global nm) -> do
                          t <- lookupDecl nm
                          case t of
                            Nothing -> abort "No le encontramos la variable global"
                            Just val -> seek val env k -- pero este val tiene tipo term, pero sabemos que es un val, y si cambiamos de modo?

destroy :: (MonadFD4 m) => Value -> Continuation -> m Value
destroy v [] = return v
destroy v (fr : k) = do 
                        -- printDestroyStatus v (fr:k) 
                        case fr of
                          PntT lit@(S st) -> do printFD4 $ st++show v
                                                destroy v k
                          PntT _          -> destroy v k
                          BOpL op term env -> seek term env (BOpR op v : k)
                          BOpR op value -> case (value, v) of
                            (VNat l, VNat r) -> destroy (VNat $ semOp op l r) k
                            _ -> abort "error de tipos runtime"
                          IfZC t e env -> case v of
                            VNat 0 -> seek t env k
                            VNat _ -> seek e env k
                            _ -> abort "error de tipos runtime"
                          AppL t env -> seek t env (AppR v : k)
                          AppR clos -> case clos of
                            CFun _ t env -> seek t (v : env) k
                            CFix _ _ t env -> seek t (clos : v : env) k
                            _ -> abort "error de tipos runtime"
                          LetD _ t env -> seek t (v : env) k -- olvido tu nombre?

eval :: (MonadFD4 m) => TTerm -> m TTerm
eval t = do v <- seek t [] []
            return $ val2TTerm v
            
-- Ayer escribimos esto
-- eval t = do
--   v <- seek t [] []
--   return (val2TTerm v)

-- Hoy se me ocurre esto
-- eval t = fmap val2TTerm $ seek t [] []

-- el linter me dice que haga esto
-- eval t = val2TTerm <$> seek t [] []


testRun :: TTerm -> IO ()
testRun t = do  resRun <- runFD4 (testRun' t) (Conf False Interactive)
                case resRun of (Right r) -> print r
                               (Left _)  -> print "Error"

testRun' :: MonadFD4 m => TTerm -> m ()
testRun' t = do 
                -- printFD4 "Comienza el run:"
                v <- seek t [] []
                printFD4 $ show v
                return ()

printSeekStatus:: (MonadFD4 m) => TTerm -> Env -> Continuation -> m()
printSeekStatus term env k = do 
          printFD4 $ show term ++  " - " ++  show env ++  " - " ++  show k
          return ()

printDestroyStatus:: (MonadFD4 m) => Value -> Continuation -> m()
printDestroyStatus v k = do 
          printFD4 $ show v  ++ " - " ++ show k
          return ()
