{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- |
-- Module      : Parse
-- Description : Define un parser de términos FD40 a términos fully named.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Parse (P, term, program, declarationOrTerm, ws, runP) where

import Common
import Control.Monad.Identity (Identity)
import Data.Char
import Data.Composition
import Data.List
import Data.List.NonEmpty (fromList)
import Data.Maybe
import Surf
import Text.Parsec hiding (parse, runP)
import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Token qualified as Tok
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
    { caseSensitive = True,
      commentStart = "#-",
      commentEnd = "-#",
      commentLine = "#",
      nestedComments = True,
      identStart = letter,
      identLetter = alphaNum <|> oneOf "_",
      opStart = langDef.opLetter,
      opLetter = oneOf $ nub . concat $ langDef.reservedOpNames,
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
          "type",
          "Nat"
        ],
      reservedOpNames =
        [ "->",
          ":",
          ";",
          "=",
          "+",
          "-",
          "!"
        ]
    }

ws :: P ()
ws = lexer.whiteSpace

nat :: P Integer
nat = lexer.natural

sLit :: P String
sLit = lexer.stringLiteral

par :: forall a. P a -> P a
par = parens lexer -- lexer.parens

res :: String -> P ()
res = lexer.reserved

op :: String -> P ()
op = lexer.reservedOp

upId :: P String
upId = do
  s <- Tok.identifier lexer
  case listToMaybe s of
    Nothing -> fail "empty identifier"
    Just c ->
      if isUpper c
        then return s
        else fail "expecting uppercase id"

lowId :: P String
lowId = do
  s <- Tok.identifier lexer
  case listToMaybe s of
    Nothing -> fail "empty identifier"
    Just c ->
      if isUpper c
        then fail "expecting lowercase id"
        else return s

-----------------------
-- Parsers
-----------------------

getPos :: P Pos
getPos = do
  pos <- getPosition
  return $ Pos (sourceLine pos) (sourceColumn pos)

numLiteral :: P Literal
numLiteral = N <$> nat

strLiteral :: P Literal
strLiteral = S <$> sLit

literal :: P Literal
literal = numLiteral <|> strLiteral

varIdent :: P Ident
varIdent = VarId <$> lowId

tyIdent :: P Ident
tyIdent = TyId <$> upId

binder :: Par -> P Binder
binder p = parens' $ do
  x <- varIdent
  op ":"
  tau <- ty
  return $ bind x tau
  where
    parens' =
      case p of
        P -> par
        NP -> id

multi :: P Multi
multi = par $ do
  xs <- fromList <$> many1 varIdent
  op ":"
  tau <- ty
  return $ bind xs tau

ty :: P Ty
ty = Ex.buildExpressionParser opTable ty' <?> "type"
  where
    opTable :: [[Ex.Operator String () Identity Ty]]
    opTable = [[binary "->" Arrow Ex.AssocRight]]
      where
        binary ::
          String -> (Ty -> Ty -> Ty) -> Ex.Assoc -> Ex.Operator String () Identity Ty
        binary s f = Ex.Infix $ lexer.reservedOp s >> return f

    ty' :: P Ty
    ty' = nat_t <|> parTy <|> alias
      where
        nat_t :: P Ty
        nat_t = res "Nat" >> return Nat <?> "nat"

        alias :: P Ty
        alias = Alias <$> tyIdent <?> "alias"

        parTy :: P Ty
        parTy = ParTy <$> par ty <?> "party"

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
        unary s o = Ex.Prefix $ op s >> return (T . UOp o)

        binary :: String -> BinaryOp -> Ex.Assoc -> Ex.Operator String () Identity Term
        binary s o = Ex.Infix $ op s >> return (T .: BOp o)

    atom :: P Term
    atom =
      T . Lit
        <$> literal -- <*> getPos
          <|> T
          . Par
        <$> par term
          <|> pnt
          <|> T
          . Var
        <$> varIdent -- <*> getPos
          <?> "atom"

    -- \| Nota el parser app también parsea un solo atom.
    app :: P Term
    app = do
      f <- atom
      args <- many atom
      return $ foldl (T .: App) f args

    pnt :: P Term
    pnt = do
      res "print"
      str <- option (S "") strLiteral
      a <- atom
      return . T $ Pnt str a

    fun :: P Term
    fun = do
      res "fun"
      bs <- fromList <$> many1 multi
      op "->"
      t <- term
      return . T $ Fun bs t

    ifz :: P Term
    ifz = do
      res "ifz"
      c <- term
      res "then"
      t <- term
      res "else"
      e <- term
      return . T $ IfZ c t e

    fix :: P Term
    fix = do
      res "fix"
      f <- binder P
      x <- binder P
      bs <- many multi
      op "->"
      t <- term
      return . T $ Fix f x bs t

    let_ :: P Term
    let_ = do
      res "let"
      core <|> rec_ <|> nRec
      where
        core :: P Term
        core = do
          b <- binder P
          (t, t') <- terms
          return . T $ Let P b NoRec [] t t'

        rec_ :: P Term
        rec_ = do
          res "rec"
          f <- varIdent
          x <- multi
          bs <- many multi
          op ":"
          tau <- ty
          (t, t') <- terms
          return . T $ Let NP (bind f tau) (Rec x) bs t t'

        nRec :: P Term
        nRec = do
          f <- varIdent
          bs <- many multi
          op ":"
          tau <- ty
          (t, t') <- terms
          return . T $ Let NP (bind f tau) NoRec bs t t'

        terms :: P (Term, Term)
        terms = do
          op "="
          t <- term
          res "in"
          t' <- term
          return (t, t')

-- \| Parser de declaraciones
declaration :: P Declaration
declaration = letDecl <|> typeDecl
  where
    typeDecl :: P Declaration
    typeDecl = do
      res "type"
      t <- tyIdent
      op "="
      tau <- ty
      return $ TypeDecl (bind t tau)

    letDecl :: P Declaration
    letDecl = do
      res "let"
      core <|> rec_ <|> nRec
      where
        core :: P Declaration
        core = do
          b <- binder P
          op "="
          t <- term
          return $ LetDecl P b NoRec [] t

        rec_ :: P Declaration
        rec_ = do
          res "rec"
          f <- varIdent
          x <- multi
          bs <- many multi
          op ":"
          tau <- ty
          op "="
          t <- term
          return $ LetDecl NP (bind f tau) (Rec x) bs t

        nRec :: P Declaration
        nRec = do
          f <- varIdent
          bs <- many multi
          op ":"
          tau <- ty
          op "="
          t <- term
          return $ LetDecl NP (bind f tau) NoRec bs t

-- | Parser de programas (listas de declaraciones)
program :: P [Declaration]
program = many declaration

-- | Parsea una declaración o un término
-- Útil para las sesiones interactivas
declarationOrTerm :: P (Either Declaration Term)
declarationOrTerm =
  Left
    <$> declaration
      <|> Right
    <$> term

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (ws *> p <* eof) () filename s

-- para debugging en uso interactivo (ghci)
parse :: P a -> String -> a
parse p s = case runP p s "" of
  Right t -> t
  Left e -> error ("no parse: " ++ show s)

-- Este para probar en ghci
test :: Show a => P a -> String -> IO ()
test parser = parseTest (ws *> parser <* eof)
