{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Scaffolding where

import Parse
import MonadFD4
import Global
import Text.Parsec
import Common
import Core
import ByteCompile
import Errors
-- |
bcc :: (MonadFD4 m) => Tm _ _ -> m Bytecode
bcc = abort "usar la de ByteCompile"

test_bcc :: Tm _ _ -> IO (Either Errors.Error ())
test_bcc tt = runFD4 (showBC <$> bcc tt >>= printFD4) $ Conf False Interactive


-- |
test_parser :: (Show a) => P a -> String -> IO ()
test_parser p = parseTest (Parse.whiteSpace *> p <* eof)
