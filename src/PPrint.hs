{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

-- |
-- Module      : PPrint
-- Description : Pretty printer para FD4.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module PPrint
  ( pp,
    ppTy,
    ppSTy,
    ppName,
    ppTypeDecl,
    ppTermDecl,
  )
where

import Common (Pos, abort)
import Core
import Data.Text (unpack)
import Global (GlEnv (termEnvironment, typeContext))
import MonadFD4 (MonadFD4, gets)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    annotate,
    defaultLayoutOptions,
    layoutSmart,
    nest,
    parens,
    sep,
    (<+>),
  )
import Prettyprinter.Render.Terminal
  ( AnsiStyle,
    Color (..),
    color,
    colorDull,
    italicized,
    renderStrict,
  )
import Subst (open, open2)
import qualified Surf as S

ty2STy :: Ty -> S.Ty
ty2STy Nat = S.Nat
ty2STy (Arrow t t') = S.Arrow (ty2STy t) (ty2STy t')

op2SOp :: BinaryOp -> S.BinaryOp
op2SOp Add = S.Add
op2SOp Sub = S.Sub

const2SConst :: Const -> S.Const
const2SConst = S.N . unN


freshen :: [Name] -> Name -> Name
freshen ns n =
  let candidates = n : map (\i -> n ++ show i) [0 ..]
  in head (filter (`notElem` ns) candidates)

-- | 'openAll' convierte términos locally nameless
-- a términos fully named abriendo todos las variables de ligadura que va encontrando
-- Debe tener cuidado de no abrir términos con nombres que ya fueron abiertos.
-- Estos nombres se encuentran en la lista ns (primer argumento).
openAll :: (i -> Pos) -> [Name] -> Tm i Var -> S.Term
openAll gp ns (Var p v) = case v of
  Bound i -> S.Var (gp p) $ "(Bound " ++ show i ++ ")" -- este caso no debería aparecer
  -- si el término es localmente cerrado
  Free x -> S.Var (gp p) x
  Global x -> S.Var (gp p) x
openAll gp ns (Cst p c) = S.Cst (gp p) (const2SConst c)
openAll gp ns (Lam p x ty t) =
  let x' = freshen ns x
  in S.Lam (gp p) [(x', ty2STy ty)] (openAll gp (x' : ns) (open x' t))
-- Si quisiésemos devolver el SLam con multibinding,
-- deberíamos llevar más info en el momento de open
openAll gp ns (App p t u) = S.App (gp p) (openAll gp ns t) (openAll gp ns u)
openAll gp ns (Fix p f fty x xty t) =
  let x' = freshen ns x
      f' = freshen (x' : ns) f
  in S.Fix (gp p) (f', ty2STy fty) [(x', ty2STy xty)] (openAll gp (x : f : ns) (open2 f' x' t))
openAll gp ns (IfZ p c t e) = S.IfZ (gp p) (openAll gp ns c) (openAll gp ns t) (openAll gp ns e)
openAll gp ns (Pnt p str t) = S.Pnt (gp p) str (openAll gp ns t)
openAll gp ns (BOp p op t u) = S.BOp (gp p) (op2SOp op) (openAll gp ns t) (openAll gp ns u)
openAll gp ns (Let p v ty m n) =
  let v' = freshen ns v
  in S.Let (gp p) (v', ty2STy ty) (openAll gp ns m) (openAll gp (v' : ns) (open v' n))

-- Colores
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (color Red)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)

keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (colorDull Green) -- <> bold)

typeColor :: Doc AnsiStyle -> Doc AnsiStyle
typeColor = annotate (color Blue <> italicized)

typeOpColor :: Doc AnsiStyle -> Doc AnsiStyle
typeOpColor = annotate (colorDull Blue)

defColor :: Doc AnsiStyle -> Doc AnsiStyle
defColor = annotate (colorDull Magenta <> italicized)

nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id

-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc n = nameColor (pretty n)

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- | Pretty printer para tipos (Doc)
sTy2Doc :: S.Ty -> Doc AnsiStyle
sTy2Doc S.Nat = typeColor (pretty "Nat")
sTy2Doc (S.Arrow x@(S.Arrow _ _) y) = sep [parens (sTy2Doc x), typeOpColor (pretty "->"), sTy2Doc y]
sTy2Doc (S.Arrow x y) = sep [sTy2Doc x, typeOpColor (pretty "->"), sTy2Doc y]
sTy2Doc (S.Alias n) = typeColor (pretty n)

-- | Pretty printer para tipos (String)
ppSTy :: S.Ty -> String
ppSTy = render . sTy2Doc

ppTy :: Ty -> String
ppTy = ppSTy . ty2STy

c2doc :: S.Const -> Doc AnsiStyle
c2doc (S.N n) = constColor (pretty (show n))

sBinary2doc :: S.BinaryOp -> Doc AnsiStyle
sBinary2doc S.Add = opColor (pretty "+")
sBinary2doc S.Sub = opColor (pretty "-")

collectApp :: S.Term -> (S.Term, [S.Term])
collectApp = go []
  where
    go ts (S.App _ h tt) = go (tt : ts) h
    go ts h = (h, ts)

parenIf :: Bool -> Doc a -> Doc a
parenIf True = parens
parenIf _ = id

-- sTerm2Doc at t :: Doc
-- at: debe ser un átomo

-- | Pretty printing de términos (Doc)
sTerm2Doc ::
  Bool -> -- Debe ser un átomo?
  S.Term -> -- término a mostrar
  Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- sTerm2Doc at x = text (show x) -}
sTerm2Doc at (S.Var _ x) = name2doc x
sTerm2Doc at (S.UOp _ _ x) = abort "unimplemented"
sTerm2Doc at (S.LetFun _ _ _ _ x) = abort "unimplemented"
sTerm2Doc at (S.LetRec _ _ _ _ x) = abort "unimplemented"
sTerm2Doc at (S.If _ x) = abort "unimplemented"
sTerm2Doc at (S.Cst _ c) = c2doc c
sTerm2Doc at (S.Lam _ [] t) = abort "unimplemented"
sTerm2Doc at (S.Lam _ [(v, ty)] t) =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "fun"),
            sBinding2doc (v, ty),
            opColor (pretty "->")
          ],
        nest 2 (sTerm2Doc False t)
      ]
sTerm2Doc at (S.Lam i ((v, ty) : bs) t) =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "fun"),
            sBinding2doc (v, ty),
            opColor (pretty "->")
          ],
        nest 2 (sTerm2Doc False (S.Lam i bs t))
      ]
sTerm2Doc at t@(S.App _ _ _) =
  let (h, ts) = collectApp t
  in parenIf at $
      sTerm2Doc True h <+> sep (map (sTerm2Doc True) ts)
sTerm2Doc at (S.Fix _ (f, fty) [(x, xty)] m) =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "fix"),
            sBinding2doc (f, fty),
            sBinding2doc (x, xty),
            opColor (pretty "->")
          ],
        nest 2 (sTerm2Doc False m)
      ]
sTerm2Doc at (S.IfZ _ c t e) =
  parenIf at $
    sep
      [ keywordColor (pretty "ifz"),
        nest 2 (sTerm2Doc False c),
        keywordColor (pretty "then"),
        nest 2 (sTerm2Doc False t),
        keywordColor (pretty "else"),
        nest 2 (sTerm2Doc False e)
      ]
sTerm2Doc at (S.Pnt _ str t) =
  parenIf at $
    sep [keywordColor (pretty "print"), pretty (show str), sTerm2Doc True t]
sTerm2Doc at (S.Let _ (v, ty) t t') =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "let"),
            sBinding2doc (v, ty),
            opColor (pretty "=")
          ],
        nest 2 (sTerm2Doc False t),
        keywordColor (pretty "in"),
        nest 2 (sTerm2Doc False t')
      ]
sTerm2Doc at (S.BOp _ o a b) =
  parenIf at $
    sTerm2Doc True a <+> sBinary2doc o <+> sTerm2Doc True b

sBinding2doc :: (Name, S.Ty) -> Doc AnsiStyle
sBinding2doc (x, sty) =
  parens (sep [name2doc x, pretty ":", sTy2Doc sty])

-- | Pretty printing de términos (String)
pp :: (MonadFD4 m) => TTerm -> m String
-- Uncomment to use the Show instance for Term
{- pp = show -}
pp t = do
  globals <- gets termEnvironment
  return (render . sTerm2Doc False $ openAll fst (map declName globals) t)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- | Pretty printing de declaraciones
ppTermDecl :: (MonadFD4 m) => Decl TTerm -> m String
ppTermDecl (Decl p x t) = do
  globals <- gets termEnvironment
  return
    ( render $
        sep
          [ defColor (pretty "let"),
            name2doc x,
            defColor (pretty "=")
          ]
          <+> nest 2 (sTerm2Doc False (openAll fst (map declName globals) t))
    )

ppTypeDecl :: (MonadFD4 m) => Decl Ty -> m String
ppTypeDecl (Decl p x ty) = do
  gdecl <- gets typeContext -- gdecl est'a muerta
  return
    ( render $
        sep
          [ defColor (pretty "type"),
            name2doc x,
            defColor (pretty "="),
            defColor (pretty (ppTy ty))
          ]
    )
