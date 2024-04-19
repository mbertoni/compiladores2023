{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- |
-- Module      : MonadFD4
-- Description : Mónada con soporte para estado, errores, e IO.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Definimos la clase de mónadas 'MonadFD4' que abstrae las mónadas con soporte para estado, errores e IO,
-- y la mónada 'FD4' que provee una instancia de esta clase.
module MonadFD4
  ( FD4,
    runFD4,
    lookupDecl,
    lookupTypeOfGlobal,
    printFD4,
    printFD4NoNewLine,
    setLastFile,
    getLastFile,
    setInter,
    getInter,
    getMode,
    getOpt,
    eraseLastFileDecls,
    failPosFD4,
    failFD4,
    addTermDecl,
    addTypeDecl,
    catchErrors,
    MonadFD4,
    module Control.Monad.Except,
    module Control.Monad.State,
    termEnvironment
  )
where

import Common
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Core
import Errors
import Global
import System.IO

  -- | TODO: Esta debería ser la forma de usar hlint, pero da ambiguous type variable
-- {-# ANN module "HLint ignore Use <$>" #-}

-- * La clase 'MonadFD4'

-- | La clase de mónadas 'MonadFD4' clasifica a las mónadas con soporte para una configuración Global 'Global.Conf',
--    para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.
--
-- Las mónadas @m@ de esta clase cuentan con las operaciones:
--   - @ask :: m Conf@
--   - @get :: m GlEnv@
--   - @put :: GlEnv -> m ()@
--   - @throwError :: Error -> m a@
--   - @catchError :: m a -> (Error -> m a) -> m a@
--   - @liftIO :: IO a -> m a@
--
-- y otras operaciones derivadas de ellas, como por ejemplo
--   - @modify :: (GlEnv -> GlEnv) -> m ()@
--   - @gets :: (GlEnv -> a) -> m a@
class (MonadIO m, MonadState GlEnv m, MonadError Error m, MonadReader Conf m) => MonadFD4 m

getOpt :: (MonadFD4 m) => m Bool
getOpt = asks optimize

getMode :: (MonadFD4 m) => m Mode
getMode = asks modo

setInter :: (MonadFD4 m) => Bool -> m ()
setInter b = modify (\s -> s {inInteractiveMode = b})

getInter :: (MonadFD4 m) => m Bool
getInter = gets inInteractiveMode

printFD4 :: (MonadFD4 m) => String -> m ()
printFD4 = liftIO . putStrLn

printFD4NoNewLine :: (MonadFD4 m) => String -> m ()
printFD4NoNewLine = liftIO . putStr

setLastFile :: (MonadFD4 m) => FilePath -> m ()
setLastFile filename = modify (\s -> s {lastFile = filename, termDeclNumber = 0, typeDeclNumber = 0})

getLastFile :: (MonadFD4 m) => m FilePath
getLastFile = gets lastFile

addTermDecl :: (MonadFD4 m) => Decl TTerm -> m ()
addTermDecl d = modify (\s -> s {termEnvironment = d : termEnvironment s, termDeclNumber = termDeclNumber s + 1})

addTypeDecl :: (MonadFD4 m) => Decl Ty -> m ()
addTypeDecl d = modify (\s -> s {typeContext = d : typeContext s, typeDeclNumber = typeDeclNumber s + 1})

addReferencedVariable :: (MonadFD4 m) => Name -> m()
addReferencedVariable x = return () -- modify (\s -> s {typeContext = d : typeContext s, typeDeclNumber = typeDeclNumber s + 1})

eraseLastFileDecls :: (MonadFD4 m) => m ()
eraseLastFileDecls = do
  s <- get
  let (_, terms) = splitAt (typeDeclNumber s) (termEnvironment s)
      (_, types) = splitAt (typeDeclNumber s) (typeContext s)
  modify (\s' -> s' {termEnvironment = terms, termDeclNumber = 0, typeContext = types, typeDeclNumber = 0})

lookupDecl :: (MonadFD4 m) => Name -> m (Maybe TTerm)
lookupDecl name = do
  s <- get
  case filter (hasName name) (termEnvironment s) of
    (Decl {body = e}) : _ -> return (Just e)
    [] -> return Nothing
  where
    hasName :: Name -> Decl a -> Bool
    hasName nm (Decl {name = nm'}) = nm == nm'

lookupTypeOfGlobal :: (MonadFD4 m) => Name -> m (Maybe Ty)
lookupTypeOfGlobal nm = do
  s <- get
  return $ lookup nm (globalTypedEnvironment s)

lookupAlias :: (MonadFD4 m) => Name -> m (Maybe Ty)
lookupAlias nm =
  gets $ lookup nm . globalTypeContext

failPosFD4 :: (MonadFD4 m) => Pos -> String -> m a
failPosFD4 p s = throwError (ErrPos p s)

failFD4 :: (MonadFD4 m) => String -> m a
failFD4 = failPosFD4 NoPos

catchErrors :: (MonadFD4 m) => m a -> m (Maybe a)
catchErrors c =
  catchError
    (Just <$> c)
    ( \e ->
        liftIO $
          hPrint stderr e >> return Nothing
    )

----
-- Importante, no eta-expandir porque GHC no hace una
-- eta-contracción de sinónimos de tipos
-- y Main no va a compilar al escribir `InputT FD4 ()`

-- | El tipo @FD4@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
-- El transformador de mónada @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
-- El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
type FD4 = ReaderT Conf (StateT GlEnv (ExceptT Error IO))

-- | Esta es una instancia vacía, ya que 'MonadFD4' no tiene funciones miembro.
instance MonadFD4 FD4

-- 'runFD4\'' corre una computación de la mónada 'FD4' en el estado inicial 'Global.initialEnv'
runFD4' :: FD4 a -> Conf -> IO (Either Error (a, GlEnv))
runFD4' c conf = runExceptT $ runStateT (runReaderT c conf) initialEnv

runFD4 :: FD4 a -> Conf -> IO (Either Error a)
runFD4 c conf = fmap fst <$> runFD4' c conf

getFresh :: MonadFD4 m => m Int
getFresh = do fr <- gets fresh
              modify (\s -> s {fresh = fr + 1})
              return fr