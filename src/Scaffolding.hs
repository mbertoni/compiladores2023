module Scaffolding where

{-
  Este tendría que ser un script para cargar en el ghci.
  Como para tener un driver interactivo, tipo nada, vistes

  tengo que aprender a usar el ghci, no puedo vivir sin lambda en el prompt ni colore

  TODO
-}

import Parse
-- import MonadFD4
-- import Global
import Text.Parsec
import Common
import Core
import ByteCompile
-- import Errors
-- |
bcc :: Term -> Bytecode
bcc = abort "usar la de ByteCompile"

-- test_bcc :: TTerm -> IO (Either Errors.Error ())
-- test_bcc tt = runFD4 (printFD4 $ showBC (bcc tt)) $ Conf False Interactive

-- |
test_parser :: (Show a) => P a -> String -> IO ()
test_parser p = parseTest (Parse.whiteSpace *> p <* eof)
