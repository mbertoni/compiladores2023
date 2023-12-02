{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : ByteCompile
-- Description : Compila a bytecode. Ejecuta bytecode.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite compilar módulos a la Macchina. También provee
-- una implementación de la Macchina para ejecutar el bytecode.
module ByteCompile (Bytecode, runBC, bcWrite, bcRead, byteCompileModule, showBC) where

import Data.Binary (Binary (get, put), Word32, decode, encode)
import Data.Binary.Get (getWord32le, isEmpty)
import Data.Binary.Put (putWord32le)
import Data.ByteString.Lazy qualified as BS
import Data.Char
import Data.List (intercalate)
import Core 
import MonadFD4
import Subst
import Common (abort)
type Opcode = Int

type Bytecode = [Int]
type Env = [Value]
type Stack = [Value]

data Value = Natural Int | Fun Env Bytecode | RetAd Env Bytecode
  deriving Show

newtype Bytecode32 = BC {un32 :: [Word32]}

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where
      go = do
        empty <- isEmpty
        if empty
          then return $ BC []
          else do
            x <- getWord32le
            BC xs <- go
            return $ BC (x : xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:

  f (CALL : cs) = ...

Notar que si hubiéramos escrito algo como
    call = 5
no podríamos hacer pattern-matching con `call`.

En lo posible, usar estos códigos exactos para poder ejecutar un
mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL = 0

pattern RETURN = 1

pattern CONST = 2

pattern ACCESS = 3

pattern FUNCTION = 4

pattern CALL = 5

pattern ADD = 6

pattern SUB = 7

pattern FIX = 9

pattern STOP = 10

pattern SHIFT = 11

pattern DROP = 12

pattern PRINT = 13

pattern PRINTN = 14

pattern JUMP = 15 

pattern TAILCALL = 16 
-- pattern JUMPTRUE = 16
-- pattern JUMPFALSE = 17

-- función util para debugging: muestra el Bytecode de forma más legible.
showOps :: Bytecode -> [String]
showOps [] = []
showOps (NULL : xs) = "NULL" : showOps xs
showOps (RETURN : xs) = "RETURN" : showOps xs
showOps (CONST : i : xs) = ("CONST " ++ show i) : showOps xs
showOps (ACCESS : i : xs) = ("ACCESS " ++ show i) : showOps xs
showOps (FUNCTION : i : xs) = ("FUNCTION len=" ++ show i) : showOps xs
showOps (CALL : xs) = "CALL" : showOps xs
showOps (TAILCALL : xs) = "TAILCALL" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (SUB : xs) = "SUB" : showOps xs
showOps (FIX : xs) = "FIX" : showOps xs
showOps (STOP : xs) = "STOP" : showOps xs
showOps (JUMP : i : xs) = ("JUMP off=" ++ show i) : showOps xs
showOps (SHIFT : xs) = "SHIFT" : showOps xs
showOps (DROP : xs) = "DROP" : showOps xs
showOps (PRINT : xs) =
  let (msg, _ : rest) = span (/= NULL) xs
  in ("PRINT " ++ show (bc2string msg)) : showOps rest
showOps (PRINTN : xs) = "PRINTN" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
-- showOps (JUMPTRUE : i : xs) = ("True off=" ++ show i) : showOps xs
-- showOps (JUMPFALSE : i : xs) = ("False off=" ++ show i) : showOps xs
showOps (x : xs) = show x : showOps xs

showBC :: Bytecode -> String
showBC = intercalate "; " . showOps

bcc ::  Term -> Bytecode
bcc (Var info (Bound i)) = ACCESS:[i] 
bcc (Lit _ (N n)) = CONST:[n] 
bcc (Lam _ _ _ (Sc1 t)) = FUNCTION:length bct:bct 
  where bct = bcTC t  
bcc (App _ t1 t2) = bct1 ++ bct2 ++ [CALL]
  where
    bct1 = bcc t1
    bct2 = bcc t2
bcc (Pnt _ (S s) t) = bcc t ++ [PRINT] ++ string2bc s ++ [NULL] ++ [PRINTN]
bcc (BOp _ Add x y) = bcx ++ bcy ++ [ADD]
  where bcx = bcc x
        bcy = bcc y
  
bcc (BOp _ Sub x y) = bcc x ++ bcc y ++ [SUB]
bcc (Fix _ fn fty x xty (Sc2 t)) = FUNCTION:[length bct] ++ bct ++ [RETURN, FIX]
  where bct = bcc t  
bcc (IfZ _ c t e) = bccond ++ (JUMP: length bcthen: bcthen) ++ (JUMP: length bcelse: bcelse) 
  where
    bccond = bcc c
    bcthen = bcc t 
    bcelse = bcc e
 
{-
Otra opción para el JUMP, usando CJUMP 
(es decir, dos JUMPS en vez de uno)

bcc (IfZ _ c t f) = do
  bccond <- bcc c
  bcthen <- bcc t 
  bcelse <- bcc e
  return $ bccond ++ [CJUMP, length bcthen] ++ bcthen ++ [JUMP, length bcelse] ++ bcelse

-}
bcc (Let _ x _ e1 (Sc1 e2)) = bce1 ++ [SHIFT] ++ bce2 ++ [DROP]
  where 
    bce1 = bcc e1
    bce2 = bcc e2
                              
bcc _ = abort "Patrón no capturado en bcc"

bcTC :: Term -> Bytecode
bcTC x@(App _ t1 t2) = bct1 ++ bct2 ++ [TAILCALL]
  where
    bct1 = bcc t1
    bct2 = bcc t2
  
bcTC x@(IfZ _ c t e) = bccond ++ (JUMP: length bcTCthen: bcTCthen) ++ (JUMP: length bcTCelse: bcTCelse)
        where
          bccond = bcc c
          bcTCthen = bcTC t 
          bcTCelse = bcTC e
bcTC x@(Let _ _ _ m (Sc1 n)) = bcm ++ [SHIFT] ++ bcn
  where 
    bcm = bcc m
    bcn = bcTC n
bcTC x = bcc x ++ [RETURN]

-- ord/chr devuelven los code-points unicode, o en otras palabras
-- la codificación UTF-32 del carácter.
string2bc :: String -> Bytecode
string2bc = map ord

bc2string :: Bytecode -> String
bc2string = map chr

byteCompileModule :: Module -> Bytecode
byteCompileModule m = bcc (getTerm $ declIntoTerm m)
  
{- 
  let x = t1
  let y = t2
  let z = t3 
  =>
  let x = t1 in (let y = t2 in t3 )
-} 
declIntoTerm :: Module -> TTerm
declIntoTerm [] = abort "Módulo vacío"
declIntoTerm [dtt] = dtt.body
declIntoTerm (dtt:dtts) = Let i dtt.name ty dtt.body (close dtt.name rest)
          where 
            rest = declIntoTerm dtts
            i = getInfo dtt.body
            ty = getTy dtt.body

-- | Toma un bytecode, lo codifica y lo escribe un archivo
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------

-- * Ejecución de bytecode

---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = (map fromIntegral <$> un32) . decode <$> BS.readFile filename

runBC :: (MonadFD4 m) => Bytecode -> m ()
runBC bc = run bc [] []


run :: MonadFD4 m => Bytecode -> Env -> Stack -> m ()
run ck@(ACCESS:i:c) e s = do  printState ck e s
                              run c e ((e!!i):s)
run ck@(CONST:n:c) e s = do printState ck e s
                            run c e (Natural n:s)
run ck@(ADD:c) e ss@(Natural n:Natural m:s) = do  printState ck e ss
                                                  run c e (Natural (m+n):s)
run ck@(SUB:c) e ss@(Natural y:Natural x:s) = do  printState ck e ss
                                                  run c e (Natural (max (x-y) 0):s) 
run ck@(CALL:c) e ss@(v:Fun ef cf:s) = do   printState ck e ss
                                            run cf (v:ef) (RetAd e c:s)  
run ck@(FUNCTION:size:c) e s = do printState ck e s
                                  printState (drop size c) e (Fun e cf:s)
                                  run (drop size c) e (Fun e cf:s)
  where cf = take size c
run ck@(PRINTN:c) e ss@(Natural n:s) = do   printState ck e ss
                                            printFD4 (show n) 
                                            run c e (Natural n:s)
run ck@(PRINT:c) e s = do printState ck e s
                          printFD4 (show strToPrint) 
                          run cDropped e s
  where strToPrint = bc2string (takeUntilNull c)  -- takeUntilNull == takeWhile  (/= NULL) ??
        cDropped = dropUntilNull c                -- dropUntilNull == dropWhile  (/= NULL) ??
run ck@(DROP:c) (v:e) s = do  printState ck (v:e) s
                              run c e s
run ck@(SHIFT:c) e ss@(v:s) = do  printState ck e ss
                                  run c (v:e) s
run ck@(JUMP:lenTrue:c) e ss@(Natural n:s) = 
  do  printState ck e ss
      if n == 0
        then run cOnlyTrue e s
        else run (drop (lenTrue +2) c) e s 
        -- Tengo que droppear el JUMPFALSE, voy directo a ejecutar 
      where cDropped  = drop (lenTrue+1) c
            cCommon   = drop (head cDropped +1) cDropped
            cOnlyTrue = take lenTrue c ++ cCommon
{-
Otra opción para el JUMP, usando CJUMP 
(es decir, dos JUMPS en vez de uno)
Si es necesario usar esta, deberíamos ver si es len+1 , len+2 y probarlo bien.
run (CJUMP:len:c) e (I n:s) = 
    if n == 0 then run c e s 
              else run (drop (len+2) c) e s 
run (JUMP:len:c) e s  = run (drop (len+2) c) e s
-}


run ck@(FIX:c) e ss@(Fun env cf:s) = do printState ck e ss
                                        run c e (Fun ef cf:s)
                                        where ef = Fun ef cf : env 
run (RETURN:_) _ ss@(v:RetAd e c:s) = run c e (v:s)
run ck@[STOP] e s = do  printState ck e s
                        -- printFD4 $ "Finnnn: " ++ showVal s
                        return ()
run (STOP:xs) e s = failFD4 "Tengo un STOP y más instrucciones"
run _ _ _ = failFD4 "Error en el ByteCode"

takeUntilNull :: Bytecode -> Bytecode
takeUntilNull [] = []
takeUntilNull (c:cs) = case c of 
                        NULL -> []
                        _ -> c : takeUntilNull cs
                      
dropUntilNull:: Bytecode -> Bytecode
dropUntilNull [] = []
dropUntilNull (c:cs) = case c of 
                        NULL -> cs
                        _ -> dropUntilNull cs

bccWithStop :: Term -> Bytecode
bccWithStop t = bcc t ++ [STOP]

-- testBC :: Term -> IO ()
-- testBC t = do res <- runFD4 ( bccWithStop t >>= printFD4 . showBC) (Conf False Interactive)
--               case res of (Right r) -> print r
--                           (Left _) -> print "Errorrrrrr"

-- testRun :: Term -> IO ()
-- testRun t = do  resRun <- runFD4 (testRun' t) (Conf False Interactive)
--                 case resRun of (Right r) -> print r
--                                (Left _)  -> print "Error"

-- testRun' :: MonadFD4 m => Term -> m ()
-- testRun' t = do bc <- bccWithStop t 
--                 -- printFD4 $ rawBC2string bc
--                 printFD4 "Comienza el run:"
--                 runBC bc

printState :: MonadFD4 m => Bytecode -> Env -> Stack -> m ()
printState c e s = do 
          -- printFD4 $ rawBC2string c
          -- printFD4 $ intercalate " - " [showBC c, showVal e, showVal s]
          return ()
                      
rawBC2string :: Bytecode -> String
rawBC2string [] = ""
rawBC2string (x:xs) = show x ++ " " ++ rawBC2string xs

showVal :: [Value] -> String
showVal (Natural n :env) = "Nat "++ show n ++ "; " ++ showVal env
showVal (Fun e bc  :env) = "Fun "++ showVal e ++ showBC bc ++ ": " ++ showVal env
showVal (RetAd e bc:env) = "Ret "++ showVal e ++ showBC bc ++ ": " ++ showVal env
showVal [] = ""

