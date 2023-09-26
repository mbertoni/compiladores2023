{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- |
-- Module      : Parse
-- Description : Define un parser de términos FD40 a términos fully named.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Parse (term, Parse.parse, runP, P, program, declarationOrTerm) where

import Common
-- import Data.Char ( isNumber, ord )

-- ( GenLanguageDef(..), emptyDef )

import Control.Monad.Identity (Identity)
import Data.Type.Equality (TestEquality)
import Surf
import Text.Parsec hiding (parse, runP)
import Text.Parsec.Expr (Assoc, Operator)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Prelude hiding (const)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef =
  emptyDef
    { commentLine = "#",
      reservedNames =
        [ "let",
          "rec",
          "fun",
          "fix",
          "then",
          "else",
          "in",
          "end",
          "ifz",
          "if",
          "print",
          "Nat",
          "type"
        ],
      reservedOpNames = ["->", ":", ";", "=", "+", "-", "!"]
    }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

numLiteral :: P Integer
numLiteral = natural

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-- identifier :: P Ident
-- identifier = Tok.identifier lexer

varIdent :: P Ident
varIdent = Tok.lexeme lexer $ do
  c <- lower
  cs <- many (identLetter langDef)
  return (c : cs)

tyIdent :: P Ident
tyIdent = Tok.lexeme lexer $ do
  c <- upper
  cs <- many (identLetter langDef)
  return (c : cs)

-----------------------
-- Parsers
-----------------------

getPos :: P Pos
getPos = do
  pos <- getPosition
  return $ Pos (sourceLine pos) (sourceColumn pos)

-- ident :: P Ident
-- ident = identifier

literal :: P Literal
literal = n <|> s
  where
    n = N <$> numLiteral
    s = S <$> stringLiteral

binder :: P Binder
binder = parens $ do
  is <- many1 varIdent
  reservedOp ":"
  t <- ty
  return (is, t)

binders :: P [Binder]
binders = many1 $ parens binder

ty :: P Ty
ty = alias <|> arrow <|> nat <|> parTy
  where
    nat :: P Ty
    nat = reserved "Nat" >> return Nat

    alias :: P Ty
    alias = Alias <$> tyIdent

    parTy :: P Ty
    parTy = ParTy <$> parens ty

    arrow :: P Ty
    arrow = do
      x <- ty
      reservedOp "->"
      y <- ty
      return (Arrow x y)

term :: P Term
term = Ex.buildExpressionParser opTable term'
  where
    term' = app <|> lam <|> ifz <|> pnt <|> fix <|> let_

    opTable :: [[Operator String () Identity Term]]
    opTable =
      [ [unary "!" Bang],
        [ binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft
        ]
      ]
      where
        unary :: String -> UnaryOp -> Operator String () Identity Term
        unary s op = Ex.Prefix $ reservedOp s >> return (T . UOp op)

        binary :: String -> BinaryOp -> Assoc -> Operator String () Identity Term
        binary s op = Ex.Infix $ reservedOp s >> return (\l r -> T (BOp op l r))

    pnt :: P Term
    pnt = do
      _ <- getPos
      reserved "print"
      str <- option "" stringLiteral
      a <- atom
      return $ T (Pnt str a)

    atom :: P Term
    atom =
      T . Lit <$> literal -- <*> getPos
        <|> T . Var <$> varIdent -- <*> getPos
        <|> parens term
        <|> pnt

    -- Nota el parser app también parsea un solo atom.
    app :: P Term
    app = do
      _ <- getPos
      f <- atom
      args <- many atom
      return (foldl (\t u -> T $ App t u) f args)

    lam :: P Term
    lam = do
      _ <- getPos
      reserved "fun"
      bs <- binders
      reservedOp "->"
      t <- term
      return (T (Lam bs t))

    ifz :: P Term
    ifz = do
      _ <- getPos
      reserved "ifz"
      c <- term
      reserved "then"
      t <- term
      reserved "else"
      e <- term
      return (T (IfZ c t e))

    fix :: P Term
    fix = do
      _ <- getPos
      reserved "fix"
      f <- binder
      x <- binder
      bs <- binders
      reservedOp "->"
      t <- term
      return (T (Fix f x bs t))

    let_ :: P Term
    let_ = core <|> nRec <|> rec_
      where
        core :: P Term
        core = do
          _ <- getPos
          reserved "let"
          (x, tau) <- parens $ do
            i <- varIdent
            reservedOp ":"
            tau <- ty
            return (i, tau)
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (T (Let P x NRec [] tau t t'))

        nRec :: P Term
        nRec = do
          _ <- getPos
          reserved "let"
          f <- varIdent
          bs <- binders
          reservedOp ":"
          tau <- ty
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (T (Let NP f NRec bs tau t t'))

        rec_ :: P Term
        rec_ = do
          _ <- getPos
          reserved "let"
          reserved "rec"
          f <- varIdent
          x <- binder
          bs <- binders
          reservedOp ":"
          tau <- ty
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (T (Let NP f (Rec x) bs tau t t'))

-- | Parser de declaraciones
declaration :: P Declaration
declaration = letDecl <|> typeDecl
  where
    typeDecl :: P Declaration
    typeDecl = do
      _ <- getPos
      reserved "type"
      i <- tyIdent
      reservedOp "="
      t <- ty
      return $ TypeDecl i t

    letDecl :: P Declaration
    letDecl = core <|> nRec <|> rec_
      where
        core :: P Declaration
        core = do
          _ <- getPos
          reserved "let"
          (x, tau) <- parens $ do
            i <- varIdent
            reservedOp ":"
            tau <- ty
            return (i, tau)
          reservedOp "="
          t <- term
          return (LetDecl P x NRec [] tau t)

        nRec :: P Declaration
        nRec = do
          _ <- getPos
          reserved "let"
          f <- varIdent
          bs <- binders
          reservedOp ":"
          tau <- ty
          reservedOp "="
          t <- term
          return (LetDecl NP f NRec bs tau t)

        rec_ :: P Declaration
        rec_ = do
          _ <- getPos
          reserved "let"
          reserved "rec"
          f <- varIdent
          x <- binder
          bs <- binders
          reservedOp ":"
          tau <- ty
          reservedOp "="
          t <- term
          return (LetDecl NP f (Rec x) bs tau t)

-- | Parser de programas (listas de declaraciones)
program :: P [Declaration]
program = many declaration

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declarationOrTerm :: P (Either Declaration Term)
declarationOrTerm =
  Left <$> declaration
    <|> Right <$> term

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

-- para debugging en uso interactivo (ghci)
parse :: String -> Term
parse s = case runP term s "" of
  Right t -> t
  Left e -> error ("no parse: " ++ show s)
