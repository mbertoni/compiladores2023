
-- |
-- Module      : Global
-- Description : Define el estado global del compilador
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Global where

import Core

data GlEnv = GlEnv
  { -- | True, si estamos en modo interactivo.
    -- Este parámetro puede cambiar durante la ejecución:
    -- Es falso mientras se cargan archivos, pero luego puede ser verdadero.
    inInteractiveMode :: Bool,
    -- | Último archivo cargado.
    lastFile :: String,
    -- | Cantidad de declaraciones tipo desde la última carga
    typeDeclNumber :: Int,
    -- | Cantidad de declaraciones término desde la última carga
    termDeclNumber :: Int,
    -- | Declaraciones de tipos
    typeContext :: [Decl Ty],
    -- | Entorno con declaraciones globales ya tipadas
    termEnvironment :: [Decl TTerm],
    -- | Variables utilizadas para el deadCode
    usedVariables :: [Name],
    -- | Contador de variables fresh
    fresh :: Int
  }

globalTypedEnvironment :: GlEnv -> [(Name, Ty)]
globalTypedEnvironment g = map (\(Decl _ n tt) -> (n, getTy tt)) g.termEnvironment

globalTypeContext :: GlEnv -> [(Name, Ty)]
globalTypeContext g = map (\(Decl _ n ty) -> (n, ty)) g.typeContext

{-
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode
  = Interactive
  | Typecheck
  | Eval
  | CEK
  | InteractiveCEK

  | Bytecompile
  | RunVM
  | CC
-- \| Canon
-- \| Assembler
-- \| Build
data Conf = Conf
  { optimize :: Bool, --  ^ True, si estan habilitadas las optimizaciones.
    modo :: Mode
  }

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv False "" 0 0 [] [] [] 0
