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
import Common
import MonadFD4
import Subst -- será que esto es una pista que nos están tirando?
import Data.Functor.Classes (eq1)

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

pattern IFZ = 16

-- función util para debugging: muestra el Bytecode de forma más legible.
showOps :: Bytecode -> [String]
showOps [] = []
showOps (NULL : xs) = "NULL" : showOps xs
showOps (RETURN : xs) = "RETURN" : showOps xs
showOps (CONST : i : xs) = ("CONST " ++ show i) : showOps xs
showOps (ACCESS : i : xs) = ("ACCESS " ++ show i) : showOps xs
showOps (FUNCTION : i : xs) = ("FUNCTION len=" ++ show i) : showOps xs
showOps (CALL : xs) = "CALL" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (SUB : xs) = "SUB" : showOps xs
showOps (FIX : xs) = "FIX" : showOps xs
showOps (IFZ : xs) = "IFZ" : showOps xs
showOps (STOP : xs) = "STOP" : showOps xs
showOps (JUMP : i : xs) = ("JUMP off=" ++ show i) : showOps xs
showOps (SHIFT : xs) = "SHIFT" : showOps xs
showOps (DROP : xs) = "DROP" : showOps xs
showOps (PRINT : xs) =
  let (msg, _ : rest) = span (/= NULL) xs
  in ("PRINT " ++ show (bc2string msg)) : showOps rest
showOps (PRINTN : xs) = "PRINTN" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (x : xs) = show x : showOps xs

showBC :: Bytecode -> String
showBC = intercalate "; " . showOps

bcc :: (MonadFD4 m) => TTerm -> m Bytecode
bcc (Var info (Bound i)) = return (ACCESS:[i]) -- Creo que las info desaparecen, again
-- bcc (Var _ (Free n)) = failFD4 "No puede haber Frees"
-- bcc (Var _ (Global n)) = failFD4 "Los TTerm deberían estar resueltos ya"
bcc (Lit _ (N n)) = return $ CONST:[n] 
-- bcc (Lit _ (S s)) = CONST:s:[] -- Appendeamos strings? Fallamos?
bcc (Lam _ _ _ (Sc1 t)) = do 
  bct <- bcc t  
  return $ FUNCTION:[length bct] ++ bct ++ [RETURN]
bcc (App _ t1 t2) = do
  bct1 <- bcc t1
  bct2 <- bcc t2
  return $ bct1 ++ bct2 ++ [CALL]
bcc (Pnt _ (N n) t) = return $ PRINTN:[n]
bcc (Pnt _ (S s) t) = do 
  bct <- bcc t
  return $ [PRINT] ++ string2bc s ++ [NULL] ++ bct
bcc (BOp _ Add x y) = do
  bcx <- bcc x
  bcy <- bcc y
  return $ bcx ++ bcy ++ [ADD]
bcc (BOp _ Sub x y) = do
  bcx <- bcc x
  bcy <- bcc y
  return $ bcx ++ bcy ++ [SUB]
bcc (Fix _ fn fty x xty (Sc2 t)) = do 
  bct <- bcc t  
  return $ FUNCTION:[length bct] ++ bct ++ [RETURN, FIX] -- No se la verdad
bcc (IfZ _ c t e) = do
  bccond <- bcc c
  bcthen <- bcc t 
  bcelse <- bcc e
  return $ bccond ++ bcthen ++ bcelse ++ [IFZ] -- No tengo idea la verdad
bcc (Let _ x _ e1 (Sc1 e2)) = do
  bce1 <- bcc e1
  bce2 <- bcc e2
  return $ bce1 ++ [SHIFT] ++ bce2 ++ [DROP]
                              
bcc _ = failFD4 "Patrón no capturado en bcc"

-- ord/chr devuelven los code-points unicode, o en otras palabras
-- la codificación UTF-32 del carácter.
string2bc :: String -> Bytecode
string2bc = map ord

bc2string :: Bytecode -> String
bc2string = map chr

type Module = [Decl TTerm] -- Creo que sería así. Definimos un programa como una lista de declaraciones
byteCompileModule :: (MonadFD4 m) => Module -> m Bytecode
byteCompileModule m = do  tt <- declIntoTerm m
                          bcc tt
                      
  
declIntoTerm :: (MonadFD4 m) => Module -> m TTerm
declIntoTerm [] = failFD4 "Módulo vacío"
-- declIntoTerm [t] = Let (t.pos) t.name Ty (Tm (t.pos) var) (Sc1 (Tm info var)) 
declIntoTerm _ = failFD4 "unimplemented"

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
run (ACCESS:i:c) e s = run c e ((e!!i):s)
run (CONST:n:c) e s = run c e (Natural n:s)
run (ADD:c) e (Natural n:Natural m:s) = run c e (Natural (m+n):s)
run (SUB:c) e (Natural y:Natural x:s) = run c e (Natural (max (x-y) 0):s) 
run (CALL:c) e (v:Fun ef cf:s) = run cf (v:ef) (RetAd e c:s)  
run (FUNCTION:size:c) e s = run (drop size c) e (Fun e cf:s)
  where cf = take size c
run (PRINTN:c) e (Natural n:s) = do printFD4 (show n) 
                                    run c e (Natural n:s)
run (PRINT:c) e s = do  printFD4 (show strToPrint) 
                        run cDropped e s
  where strToPrint = bc2string (takeUntilNull c)
        cDropped = dropUntilNull c
run (DROP:c) (v:e) s = run c e s
run (SHIFT:c) e (v:s) = run c (v:e) s
run (IFZ:c) e s = run c e s
run (JUMP:c) e s = failFD4 "Unimplemented Jump"
run (FIX:c) e (Fun env cf:s) = run c e (Fun ef cf:s)
  where ef = Fun ef cf : env 
run (RETURN:_) _ (v:RetAd e c:s) = run c e (v:s)
run (STOP:_) _ _ = return ()
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

