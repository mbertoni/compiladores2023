-- {-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Global
Description : Define el estado global del compilador
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module Global where

import Lang


data GlEnv = GlEnv {
  inInteractiveMode :: Bool,          --  ^ True, si estamos en modo interactivo.
                          -- Este parámetro puede cambiar durante la ejecución:
                          -- Es falso mientras se cargan archivos, pero luego puede ser verdadero.
  lastFile :: String,        -- ^ Último archivo cargado.
  typeDeclNumber :: Int,        -- ^ Cantidad de declaraciones tipo desde la última carga
  termDeclNumber :: Int,        -- ^ Cantidad de declaraciones término desde la última carga
  typeContext  :: [Decl Ty], -- ^ Declaraciones de tipos
  termEnvironment :: [Decl TTerm]     -- ^ Entorno con declaraciones globales ya tipadas
}

-- ^ Entorno de tipado de declaraciones globales
globalTypedEnvironment :: GlEnv ->  [(Name, Ty)]
globalTypedEnvironment g = map (\(Decl _ n tt) -> (n, getTy tt)) (termEnvironment g)

globalTypeContext :: GlEnv ->  [(Name, Ty)]
globalTypeContext g = map (\(Decl _ n ty) -> (n, ty)) (typeContext g)

{-
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode =
    Interactive
  | Typecheck
  | Eval
  -- | InteractiveCEK
  -- | Bytecompile
  -- | RunVM
  -- | CC
  -- | Canon
  -- | Assembler
  -- | Build
data Conf = Conf {
    optimize   :: Bool,          --  ^ True, si estan habilitadas las optimizaciones.
    modo  :: Mode
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv False "" 0 0 [] []
