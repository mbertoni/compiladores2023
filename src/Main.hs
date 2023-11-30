-- |
-- Module      : Main
-- Description : Compilador de FD4.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Main where

-- import Control.Monad

import qualified CEK 
-- import Common 
import Control.Exception (IOException, catch)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans
import qualified Core as C
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import Elab (declaration, term, ident)
import Errors
import Eval (eval)
import Global
import MonadFD4
import Options.Applicative
import PPrint (ppTTerm, ppTermDecl, ppTy, ppTypeDecl)
import Parse (P, runP, declarationOrTerm, program, term)
import qualified Surf as S
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    runInputT,
  )
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import TypeChecker (tc, tcDecl)
-- import ByteCompile
-- import Optimizer

prompt :: String
prompt = "FD4> "

-- | Parser de banderas
parseMode :: Parser (Mode, Bool)
parseMode =
  (,)
    <$> ( flag'
            Typecheck
            ( long "typecheck"
                <> short 't'
                <> help "Chequear tipos e imprimir el término"
            )
            <|> flag'
              InteractiveCEK
              ( long "icek"
                  <> short 'k'
                  <> help "Ejecutar de forma interactiva en la CEK"
              )
            -- <|> flag' Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
            -- <|> flag' RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
            <|> flag
              Interactive
              Interactive
              ( long "interactive"
                  <> short 'i'
                  <> help "Ejecutar en forma interactiva"
              )
            <|> flag Eval Eval (long "eval" <> short 'e' <> help "Evaluar un programa")
            <|> flag CEK CEK (long "cek" <> short 'k' <> help "Evaluar un programa con la CEK")
        )
    -- <|> flag' CC ( long "cc" <> short 'c' <> help "Compilar a código C")  <- sería acá lo que hay que descomentar?
    -- <|> flag' Canon ( long "canon" <> short 'n' <> help "Imprimir canonización")
    -- <|> flag' Assembler ( long "assembler" <> short 'a' <> help "Imprimir Assembler resultante")
    -- <|> flag' Build ( long "build" <> short 'b' <> help "Compilar")
    <*> pure False

-- reemplazar por la siguiente línea para habilitar opción
-- <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode, Bool, [FilePath])
parseArgs =
  (\(a, b) c -> (a, b, c))
    <$> parseMode
    <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "Compilador de FD4"
            <> header "Compilador de FD4 de la materia Compiladores 2023"
        )

    go :: (Mode, Bool, [FilePath]) -> IO ()
    go (Interactive, opt, files)  = runOrFail (Conf opt Interactive) (runInputT defaultSettings (repl files))
    -- go (InteractiveCEK, opt, files)  = runOrFail (Conf opt Interactive) (runInputT defaultSettings (repl files))
    -- go (RunVM, opt, files)        = runOrFail (Conf opt RunVM)        $ mapM_ runVM files
    -- go (CC, opt, files)           = runOrFail (Conf opt CC)           $ mapM_ compileC files
    -- go (Bytecompile, opt, files)  = runOrFail (Conf opt Bytecompile)  $ mapM_ bytecompile files
    go (m, opt, files)            = runOrFail (Conf opt m)            $ mapM_ compileFile files

runOrFail :: Conf -> FD4 a -> IO a
runOrFail c m = do
  r <- runFD4 m c
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => [FilePath] -> InputT m ()
repl args = do
  lift $ setInter True
  lift $ catchErrors $ mapM_ compileFile args
  s <- lift get
  when (inInteractiveMode s) $
    liftIO $
      putStrLn
        ("Entorno interactivo para FD4.\n" ++ "Escriba :? para recibir ayuda.")
  loop
  where
    loop = do
      input <- getInputLine prompt
      case input of
        Nothing -> return ()
        Just "" -> loop
        Just x -> do
          c <- liftIO $ interpretCommand x
          b <- lift $ catchErrors $ handleCommand c
          maybe loop (`when` loop) b

loadFile :: (MonadFD4 m) => FilePath -> m [S.Declaration]
loadFile f = do
  let filename = reverse (dropWhile isSpace (reverse f))
  x <-
    liftIO $
      catch
        (readFile filename)
        ( \e -> do
            let err = show (e :: IOException)
            hPutStrLn
              stderr
              ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
            return ""
        )
  setLastFile filename
  parseIO filename program x

compileFile :: (MonadFD4 m) => FilePath -> m ()
compileFile f = do
  i <- getInter
  setInter False
  when i $ printFD4 ("Abriendo " ++ f ++ "...")
  declarations <- loadFile f
  mapM_ handleDeclaration declarations
  setInter i

parseIO :: (MonadFD4 m) => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ParseErr e)
  Right r -> return r

evalDecl :: (MonadFD4 m) => C.Decl C.TTerm -> m (C.Decl C.TTerm)
evalDecl (C.Decl p x e) = do
  e' <- eval e
  return (C.Decl p x e')

handleDeclaration :: (MonadFD4 m) => S.Declaration -> m ()
handleDeclaration d = do
  m <- getMode
  gamma <- gets globalTypeContext
  let elaborated = Elab.declaration gamma d
  case m of
    Eval -> case elaborated of
      Left (C.Decl p x tm) -> do
        -- printFD4 ("\nBefore Elabing: " ++ show d)
        -- printFD4 ("\nEnvironment: " ++ show gamma)
        -- printFD4 ("\nRaw: " ++ show tm)
        tt <- tcDecl (C.Decl p x tm)
        -- printFD4 ("\nTypeChecked: " ++ show tt)
        -- printFD4 "\nEvaling: "
        te <- eval (C.body tt)
        -- printFD4 ("\nAfter Evaling: " ++ show te)
        addTermDecl (C.Decl p x te)
      Right (C.Decl p x ty) -> addTypeDecl (C.Decl p x ty)
    CEK -> case elaborated of -- TODO Es un compilador mono-comando, como la canilla!!!
      Left (C.Decl p x tm) -> do
        -- printFD4 ("\nBefore Elabing: " ++ show d)
        -- printFD4 ("\nEnvironment: " ++ show gamma)
        -- printFD4 ("\nRaw: " ++ show tm)
        tt <- tcDecl (C.Decl p x tm)
        -- printFD4 ("\nTypeChecked: " ++ show tt)
        -- printFD4 "\nEvaling: "
        te <- CEK.eval (C.body tt)
        -- printFD4 ("\nAfter Evaling: " ++ show te)
        addTermDecl (C.Decl p x te)
      Right (C.Decl p x ty) -> addTypeDecl (C.Decl p x ty)
    Interactive -> case elaborated of
      Left (C.Decl p x tm) -> do
        tt <- tcDecl (C.Decl p x tm)
        te <- eval (C.body tt)
        addTermDecl (C.Decl p x te)
      Right (C.Decl p x ty) -> 
        addTypeDecl (C.Decl p x ty)
    Typecheck -> do
      f <- getLastFile
      printFD4 ("Chequeando tipos de " ++ f)
      case elaborated of
        Left (C.Decl p x tm) -> do
          printFD4 ("\nTypechecking")
          tt <- tcDecl (C.Decl p x tm)
          addTermDecl tt
          ppterm <- ppTermDecl tt
          printFD4 ppterm
        Right (C.Decl p x ty) -> do
          addTypeDecl (C.Decl p x ty)
          ppty <- ppTypeDecl (C.Decl p x ty)
          printFD4 ppty
    -- opt <- getOpt
    -- td' <- if opt then optimize td else td
    _ -> return ()

-- do
-- td <- typecheckDecl d
-- -- td' <- if opt then optimizeDecl td else return td
-- ed <- evalDecl td
-- addDecl ed
data Command
  = Compile CompileForm
  | PPrint String
  | Type String
  | Reload
  | Browse
  | Quit
  | Help
  | Noop

data CompileForm
  = CompileInteractive String
  | CompileFile String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x =
  if ":" `isPrefixOf` x
    then do
      let (cmd, t') = break isSpace x
          t = dropWhile isSpace t'
      --  find matching commands
      let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
      case matching of
        [] -> do
          putStrLn
            ( "Comando desconocido `"
                ++ cmd
                ++ "'. Escriba :? para recibir ayuda."
            )
          return Noop
        [Cmd _ _ f _] -> return (f t)
        _ -> do
          putStrLn
            ( "Comando ambiguo, podría ser "
                ++ intercalate ", " ([head cs | Cmd cs _ _ _ <- matching])
                ++ "."
            )
          return Noop
    else return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope",
    Cmd
      [":load"]
      "<file>"
      (Compile . CompileFile)
      "Cargar un programa desde un archivo",
    Cmd
      [":print"]
      "<exp>"
      PPrint
      "Imprime un término y sus ASTs sin evaluarlo",
    Cmd
      [":reload"]
      ""
      (const Reload)
      "Vuelve a cargar el último archivo cargado",
    Cmd [":type"] "<exp>" Type "Chequea el tipo de una expresión",
    Cmd [":quit", ":Q"] "" (const Quit) "Salir del intérprete",
    Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer carácter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "let <var> = <expr>      definir una variable\n"
    ++ unlines
      ( map
          ( \(Cmd c a _ d) ->
              let ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
              in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
          )
          cs
      )

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand :: (MonadFD4 m) => Command -> m Bool
handleCommand cmd = do
  s@GlEnv {..} <- get
  case cmd of
    Quit -> return False
    Noop -> return True
    Help -> printFD4 (helpTxt commands) >> return True
    Browse -> do
      printFD4 (unlines (reverse (nub (map C.name termEnvironment))))
      return True
    Compile c -> do
      case c of
        CompileInteractive e -> compilePhrase e
        CompileFile f -> compileFile f
      return True
    Reload ->
      eraseLastFileDecls >> (getLastFile >>= compileFile) >> return True
    PPrint e -> printPhrase e >> return True
    Type e -> typeCheckPhrase e >> return True

compilePhrase :: (MonadFD4 m) => String -> m ()
compilePhrase x = do
  dot <- parseIO "<interactive>" declarationOrTerm x
  case dot of
    Left d -> handleDeclaration d
    Right t -> handleTerm t

handleTerm :: (MonadFD4 m) => S.Term -> m ()
handleTerm t = do
  s <- get
  let _t = Elab.term (globalTypeContext s) t
  tt <- tc _t (globalTypedEnvironment s)
  te <- eval tt
  printout <- ppTTerm te
  printFD4 (printout ++ " : " ++ ppTy (C.getTy tt))

printPhrase :: (MonadFD4 m) => String -> m ()
printPhrase input = do
  surfTerm <- parseIO "<interactive>" Parse.term input
  gamma <- gets globalTypeContext
  let coreTerm = Elab.term gamma surfTerm
  globals <- gets globalTypedEnvironment
  coreTTerm <- tc coreTerm globals
  t <- case surfTerm of
    (S.T (S.Var f)) -> fromMaybe coreTTerm <$> lookupDecl (ident f)
    _ -> return coreTTerm
  printFD4 "Surf Term:"
  printFD4 (show surfTerm)
  printFD4 "Core TTerm:"
  printFD4 (show t)

typeCheckPhrase :: (MonadFD4 m) => String -> m ()
typeCheckPhrase x = do
  t <- parseIO "<interactive>" Parse.term x
  s <- get
  let t' = Elab.term (globalTypeContext s) t
  tt <- tc t' (globalTypedEnvironment s)
  let ty = C.getTy tt
  printFD4 (ppTy ty)
