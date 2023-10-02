{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- |
-- Module      : Parse
-- Description : Define un parser de términos FD40 a términos fully named.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Parse (P, term, program, declarationOrTerm, whiteSpace) where

import Common
import Control.Monad.Identity (Identity)
import Data.Char
import Data.Composition
import Data.List.NonEmpty (fromList)
import Surf
import Text.Parsec hiding (parse, runP)
import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec.Language
import Prelude

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

numLiteral :: P Literal
numLiteral = N <$> natural

stringLiteral :: P Literal
stringLiteral = S <$> Tok.stringLiteral lexer

literal :: P Literal
literal = numLiteral <|> stringLiteral

varIdent :: P Ident
varIdent = do
  s <- Tok.identifier lexer
  if isUpper (head s)
    then fail "expecting lowercase id"
    else return (VarId s)

tyIdent :: P Ident
tyIdent = do
  s <- Tok.identifier lexer
  if isUpper (head s)
    then return (TyId s)
    else fail "expecting uppercase id"

binder :: Par -> P Binder
binder p = parens' $ do
  x <- varIdent
  reservedOp ":"
  tau <- ty
  return $ bind x tau
  where
    parens' =
      case p of
        P -> parens
        NP -> id

multi :: P Multi
multi = parens $ do
  xs <- fromList <$> many1 varIdent
  reservedOp ":"
  tau <- ty
  return $ bind xs tau

ty :: P Ty
ty = Ex.buildExpressionParser opTable ty' <?> "type"
  where
    opTable :: [[Ex.Operator String () Identity Ty]]
    opTable = [[binary "->" Arrow Ex.AssocRight]]
      where
        binary :: String -> (Ty -> Ty -> Ty) -> Ex.Assoc -> Ex.Operator String () Identity Ty
        binary s f = Ex.Infix $ reservedOp s >> return f

    ty' :: P Ty
    ty' = nat <|> parTy <|> alias
      where
        nat :: P Ty
        nat = reserved "Nat" >> return Nat <?> "nat"

        alias :: P Ty
        alias = Alias <$> tyIdent <?> "alias"

        parTy :: P Ty
        parTy = ParTy <$> parens ty <?> "party"

term :: P Term
term = Ex.buildExpressionParser opTable term' <?> "term"
  where
    term' :: P Term
    term' = ifz <|> fun <|> fix <|> let_ <|> app

    opTable :: [[Ex.Operator String () Identity Term]]
    opTable =
      [ [unary "!" Bang],
        [ binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft
        ]
      ]
      where
        unary :: String -> UnaryOp -> Ex.Operator String () Identity Term
        unary s op = Ex.Prefix $ reservedOp s >> return (T . UOp op)

        binary :: String -> BinaryOp -> Ex.Assoc -> Ex.Operator String () Identity Term
        binary s op = Ex.Infix $ reservedOp s >> return (T .: BOp op)

    atom :: P Term
    atom =
      T . Lit <$> literal -- <*> getPos
        <|> T . Par <$> parens term
        <|> pnt
        <|> T . Var <$> varIdent -- <*> getPos
        <?> "atom"

    -- \| Nota el parser app también parsea un solo atom.
    app :: P Term
    app = do
      f <- atom
      args <- many atom
      return $ foldl (T .: App) f args

    pnt :: P Term
    pnt = do
      reserved "print"
      str <- option (S "") stringLiteral
      a <- atom
      return . T $ Pnt str a

    fun :: P Term
    fun = do
      reserved "fun"
      bs <- fromList <$> many1 multi
      reservedOp "->"
      t <- term
      return . T $ Fun bs t

    ifz :: P Term
    ifz = do
      reserved "ifz"
      c <- term
      reserved "then"
      t <- term
      reserved "else"
      e <- term
      return . T $ IfZ c t e

    fix :: P Term
    fix = do
      reserved "fix"
      f <- binder P
      x <- binder P
      bs <- many multi
      reservedOp "->"
      t <- term
      return . T $ Fix f x bs t

    let_ :: P Term
    let_ = do
      reserved "let"
      core <|> rec_ <|> nRec
      where
        core :: P Term
        core = do
          b <- binder P
          (t, t') <- terms
          return . T $ Let P b NoRec [] t t'

        rec_ :: P Term
        rec_ = do
          reserved "rec"
          f <- varIdent
          x <- multi
          bs <- many multi
          reservedOp ":"
          tau <- ty
          (t, t') <- terms
          return . T $ Let NP (bind f tau) (Rec x) bs t t'

        nRec :: P Term
        nRec = do
          f <- varIdent
          bs <- many multi
          reservedOp ":"
          tau <- ty
          (t, t') <- terms
          return . T $ Let NP (bind f tau) NoRec bs t t'

        terms :: P (Term, Term)
        terms = do
          reservedOp "="
          t <- term
          reserved "in"
          t' <- term
          return (t, t')

-- \| Parser de declaraciones
declaration :: P Declaration
declaration = letDecl <|> typeDecl
  where
    typeDecl :: P Declaration
    typeDecl = do
      reserved "type"
      t <- tyIdent
      reservedOp "="
      tau <- ty
      return $ TypeDecl (bind t tau)

    letDecl :: P Declaration
    letDecl = do
      reserved "let"
      core <|> rec_ <|> nRec
      where
        core :: P Declaration
        core = do
          b <- binder P
          reservedOp "="
          t <- term
          return $ LetDecl P b NoRec [] t

        rec_ :: P Declaration
        rec_ = do
          reserved "rec"
          f <- varIdent
          x <- multi
          bs <- many multi
          reservedOp ":"
          tau <- ty
          reservedOp "="
          t <- term
          return $ LetDecl NP (bind f tau) (Rec x) bs t

        nRec :: P Declaration
        nRec = do
          f <- varIdent
          bs <- many multi
          reservedOp ":"
          tau <- ty
          reservedOp "="
          t <- term
          return $ LetDecl NP (bind f tau) NoRec bs t

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
parse :: P a -> String -> a
parse p s = case runP p s "" of
  Right t -> t
  Left e -> error ("no parse: " ++ show s)
