module CEK (eval) where

import Common
import Core
import Data.Default (def)
import Eval (semOp)
import MonadFD4
import Global

data Value
  = VNat Int
  | CFun Name TTerm Env
  | CFix Name Name TTerm Env
  deriving (Show)

lit2Value :: Literal -> Value
lit2Value (N i) = VNat i
lit2Value (S s) = abort "not implemented"

val2TTerm :: Value -> TTerm
val2TTerm (VNat i) = Lit (def, Nat) (N i)
val2TTerm _ = abort "immediate value expected"

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
seek term env k = do  printSeekStatus term env k 
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
destroy v (fr : k) = do printDestroyStatus v (fr:k) 
                        case fr of
                          PntT lit -> printFD4 (unS lit ++ show v) >> destroy v k
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
-- Ayer escribimos esto
-- eval t = do
--   v <- seek t [] []
--   return (val2TTerm v)

-- Hoy se me ocurre esto
-- eval t = fmap val2TTerm $ seek t [] []

-- el linter me dice que haga esto
eval t = val2TTerm <$> seek t [] []


testRun :: TTerm -> IO ()
testRun t = do  resRun <- runFD4 (testRun' t) (Conf False Interactive)
                case resRun of (Right r) -> print r
                               (Left _)  -> print "Error"

testRun' :: MonadFD4 m => TTerm -> m ()
testRun' t = do -- bc <- bccWithStop t 
                -- printFD4 $ rawBC2string bc
                printFD4 "Comienza el run:"
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
                      
-- rawBC2string :: Bytecode -> String
-- rawBC2string [] = ""
-- rawBC2string (x:xs) = show x ++ " " ++ rawBC2string xs


d :: (Pos,Ty)
d = (def::Common.Pos, Nat)

-- tc1 = Lam d "x" Nat (Sc1 (BOp d Add (Var d (Bound 0)) (4))) -- \x -> x+4
-- tc2 = (App d 5 (Lam d "x" Nat (Sc1 (BOp d Add (Var d (Bound 0)) (4))) )  )
tc3 = Let d "x" Nat 4 (Sc1 (BOp d Add (Var d (Bound 0)) 9)) -- let x = 4 in x+9
tc4 = Lam d "x" Nat (Sc1 (BOp d Add (Var d (Bound 0) ) 9) ) -- \x -> x+9
tc5 = App d tc4 5
tc6 = Pnt d (S "pastito") tc3
tc7 = BOp d Sub 9 8
tc8 = Pnt d (S "verde") tc6
tc9 = IfZ d 0 1 2
tc10 = IfZ d 1 1 2
tc11 = IfZ d (BOp d Add 2 3) (Pnt d (S "True") 1) (Pnt d (S "False") 2)
tc12 = IfZ d 0 (Pnt d (S "True") (BOp d Add 2 3)) (Pnt d (S "False") 2)
tc13 = IfZ d 1 (Pnt d (S "True") (BOp d Add 2 3)) (Pnt d (S "False") 2)
tc14 = IfZ d (Pnt d (S "Condicion") (BOp d Add 2 3)) (Pnt d (S "True") (BOp d Add 2 7)) (Pnt d (S "False") 2)
tc15 = IfZ d 0 tc3 tc7
tc16 = IfZ d 1 tc3 tc7
tc17 = Pnt d (S "pastito") tc15
tc18 = Pnt d (S "pastito") tc16

