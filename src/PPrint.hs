{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-|
Module      : PPrint
Description : Pretty printer para FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module PPrint (
    pp,
    ppTy,
    ppSTy,
    ppName,
    ppDecl
    ) where

import Lang
import Subst ( open, open2 )
import Common ( Pos, abort )

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
    ( (<+>),
      annotate,
      defaultLayoutOptions,
      layoutSmart,
      nest,
      sep,
      parens,
      Doc,
      Pretty(pretty) )
import MonadFD4 ( gets, MonadFD4 )
import Global ( GlEnv(termEnvironment) )

ty2STy :: Ty -> STy
ty2STy NatTy = SNatTy
ty2STy (FunTy t t') = SFunTy (ty2STy t) (ty2STy t')

freshen :: [Name] -> Name -> Name
freshen ns n = let cands = n : map (\i -> n ++ show i) [0..] 
               in head (filter (`notElem` ns) cands)

-- | 'openAll' convierte términos locally nameless
-- a términos fully named abriendo todos las variables de ligadura que va encontrando
-- Debe tener cuidado de no abrir términos con nombres que ya fueron abiertos.
-- Estos nombres se encuentran en la lista ns (primer argumento).
openAll :: (i -> Pos) -> [Name] -> Tm i Var -> STerm
openAll gp ns (V p v) = case v of 
      Bound i ->  SV (gp p) $ "(Bound "++show i++")" --este caso no debería aparecer
                                               --si el término es localmente cerrado
      Free x -> SV (gp p) x
      Global x -> SV (gp p) x
openAll gp ns (Const p c) = SConst (gp p) c
openAll gp ns (Lam p x ty t) = 
  let x' = freshen ns x 
  in SLam (gp p) [(x', ty2STy ty)] (openAll gp (x':ns) (open x' t))
  -- Si quisiésemos devolver el SLam con multibinding, 
  -- deberíamos llevar más info en el momento de open
openAll gp ns (App p t u) = SApp (gp p) (openAll gp ns t) (openAll gp ns u)
openAll gp ns (Fix p f fty x xty t) = 
  let 
    x' = freshen ns x
    f' = freshen (x':ns) f
  in SFix (gp p) (f', ty2STy fty) [(x', ty2STy xty)] (openAll gp (x:f:ns) (open2 f' x' t))
openAll gp ns (IfZ p c t e) = SIfZ (gp p) (openAll gp ns c) (openAll gp ns t) (openAll gp ns e)
openAll gp ns (Print p str t) = SPrint (gp p) str (openAll gp ns t)
openAll gp ns (BinaryOp p op t u) = SBinaryOp (gp p) op (openAll gp ns t) (openAll gp ns u)
openAll gp ns (Let p v ty m n) = 
    let v'= freshen ns v 
    in  SLet (gp p) (v', ty2STy ty) (openAll gp ns m) (openAll gp (v':ns) (open v' n))

--Colores
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
sTy2Doc :: STy -> Doc AnsiStyle
sTy2Doc SNatTy = typeColor (pretty "Nat")
sTy2Doc (SFunTy x@(SFunTy _ _) y) = sep [parens (sTy2Doc x), typeOpColor (pretty "->"), sTy2Doc y]
sTy2Doc (SFunTy x y) = sep [sTy2Doc x, typeOpColor (pretty "->"),sTy2Doc y]
sTy2Doc (SVar n) = typeColor (pretty n)

-- | Pretty printer para tipos (String)
ppSTy :: STy -> String
ppSTy = render . sTy2Doc

ppTy :: Ty -> String
ppTy = ppSTy . ty2STy

c2doc :: Const -> Doc AnsiStyle
c2doc (CNat n) = constColor (pretty (show n))

binary2doc :: BinaryOp -> Doc AnsiStyle
binary2doc Add = opColor (pretty "+")
binary2doc Sub = opColor (pretty "-")

collectApp :: STerm -> (STerm, [STerm])
collectApp = go [] where
  go ts (SApp _ h tt) = go (tt:ts) h
  go ts h = (h, ts)

parenIf :: Bool -> Doc a -> Doc a
parenIf True = parens
parenIf _ = id

-- sTerm2Doc at t :: Doc
-- at: debe ser un átomo
-- | Pretty printing de términos (Doc)
sTerm2Doc :: Bool     -- Debe ser un átomo? 
      -> STerm    -- término a mostrar
      -> Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- sTerm2Doc at x = text (show x) -}
sTerm2Doc at (SV _ x) = name2doc x
sTerm2Doc at (SUnaryOp _ _ x) = abort "unimplemented"
sTerm2Doc at (SLetFun _ _ _ _ x) = abort "unimplemented"
sTerm2Doc at (SLetRec _ _ _ _ x) = abort "unimplemented"
sTerm2Doc at (SIf _ x) = abort "unimplemented"
sTerm2Doc at (SConst _ c) = c2doc c
sTerm2Doc at (SLam _ [] t) = abort "unimplemented"
sTerm2Doc at (SLam _ [(v,ty)] t) =
  parenIf at $
  sep [sep [ keywordColor (pretty "fun")
           , sBinding2doc (v, ty)
           , opColor(pretty "->")]
      , nest 2 (sTerm2Doc False t)]
sTerm2Doc at (SLam i ((v,ty):bs) t) = 
  parenIf at $
  sep [sep [ keywordColor (pretty "fun")
           , sBinding2doc (v, ty)
           , opColor(pretty "->")]
      , nest 2 (sTerm2Doc False (SLam i bs t ))]


sTerm2Doc at t@(SApp _ _ _) =
  let (h, ts) = collectApp t in
  parenIf at $
  sTerm2Doc True h <+> sep (map (sTerm2Doc True) ts)

sTerm2Doc at (SFix _ (f,fty) [(x,xty)] m) =
  parenIf at $
  sep [ sep [keywordColor (pretty "fix")
                  , sBinding2doc (f, fty)
                  , sBinding2doc (x, xty)
                  , opColor (pretty "->") ]
      , nest 2 (sTerm2Doc False m)
      ]
sTerm2Doc at (SIfZ _ c t e) =
  parenIf at $
  sep [keywordColor (pretty "ifz"), nest 2 (sTerm2Doc False c)
     , keywordColor (pretty "then"), nest 2 (sTerm2Doc False t)
     , keywordColor (pretty "else"), nest 2 (sTerm2Doc False e) ]

sTerm2Doc at (SPrint _ str t) =
  parenIf at $
  sep [keywordColor (pretty "print"), pretty (show str), sTerm2Doc True t]

sTerm2Doc at (SLet _ (v,ty) t t') =
  parenIf at $
  sep [
    sep [keywordColor (pretty "let")
       , sBinding2doc (v,ty)
       , opColor (pretty "=") ]
  , nest 2 (sTerm2Doc False t)
  , keywordColor (pretty "in")
  , nest 2 (sTerm2Doc False t') ]

sTerm2Doc at (SBinaryOp _ o a b) =
  parenIf at $
  sTerm2Doc True a <+> binary2doc o <+> sTerm2Doc True b

sBinding2doc :: (Name, STy) -> Doc AnsiStyle
sBinding2doc (x, sty) =
  parens (sep [name2doc x, pretty ":", sTy2Doc sty])

-- | Pretty printing de términos (String)
pp :: MonadFD4 m => TTerm -> m String
-- Uncomment to use the Show instance for Term
{- pp = show -}
pp t = do
       gdecl <- gets termEnvironment
       return (render . sTerm2Doc False $ openAll fst (map declName gdecl) t)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- | Pretty printing de declaraciones
ppDecl :: MonadFD4 m => Decl TTerm -> m String
ppDecl (Decl p x t) = do 
  gdecl <- gets termEnvironment
  return (render $ sep [defColor (pretty "let")
                       , name2doc x 
                       , defColor (pretty "=")] 
                   <+> nest 2 (sTerm2Doc False (openAll fst (map declName gdecl) t)))
                         

