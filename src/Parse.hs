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

parens :: P a -> P a
parens = Tok.parens lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

getPos :: P Pos
getPos = do
  pos <- getPosition
  return $ Pos (sourceLine pos) (sourceColumn pos)

-- ident :: P Ident
-- ident = identifier

numLiteral :: P Literal
numLiteral = N <$> natural

stringLiteral :: P Literal
stringLiteral = S <$> Tok.stringLiteral lexer

literal :: P Literal
literal = numLiteral <|> stringLiteral

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

binder :: P Binder
binder = parens $ do
  x <- varIdent
  reservedOp ":"
  tau <- ty
  return (x, tau)

multiBinder :: P MultiBinder
multiBinder = many1 $ parens $ do
  x <- many1 varIdent
  reservedOp ":"
  tau <- ty
  return (x, tau)


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
      str <- option (S "") stringLiteral
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
      bs <- multiBinder
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
      bs <- multiBinder
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
          b <- parens $ do
            x <- varIdent
            reservedOp ":"
            tau <- ty
            return $ bind x tau
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (T (Let P b NRec [] t t'))

        nRec :: P Term
        nRec = do
          _ <- getPos
          reserved "let"
          f <- varIdent
          bs <- multiBinder
          reservedOp ":"
          tau <- ty
          let b = bind f tau
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (T (Let NP b NRec bs t t'))

        rec_ :: P Term
        rec_ = do
          _ <- getPos
          reserved "let"
          reserved "rec"
          f <- varIdent
          x <- binder
          bs <- multiBinder
          reservedOp ":"
          tau <- ty
          let b = bind f tau
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (T (Let NP b (Rec x) bs t t'))

-- | Parser de declaraciones
declaration :: P Declaration
declaration = letDecl <|> typeDecl
  where
    typeDecl :: P Declaration
    typeDecl = do
      _ <- getPos
      reserved "type"
      t <- tyIdent
      reservedOp "="
      tau <- ty
      return $ TypeDecl (bind t tau)

    letDecl :: P Declaration
    letDecl = core <|> nRec <|> rec_
      where
        core :: P Declaration
        core = do
          _ <- getPos
          reserved "let"
          b <- parens $ do
            x <- varIdent
            reservedOp ":"
            tau <- ty
            return $ bind x tau
          reservedOp "="
          t <- term
          return $ LetDecl P b NRec [] t

        nRec :: P Declaration
        nRec = do
          _ <- getPos
          reserved "let"
          f <- varIdent
          bs <- multiBinder
          reservedOp ":"
          tau <- ty
          let b = bind f tau
          reservedOp "="
          t <- term
          return $ LetDecl NP b NRec bs t

        rec_ :: P Declaration
        rec_ = do
          _ <- getPos
          reserved "let"
          reserved "rec"
          f <- varIdent
          x <- binder
          bs <- multiBinder
          reservedOp ":"
          tau <- ty
          let b = bind f tau
          reservedOp "="
          t <- term
          return $ LetDecl NP b (Rec x) bs t

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
