module C (ir2C) where
import Common (abort)
import Core hiding (name, pretty)
import Data.Char (isAlpha, ord)
import Data.String (fromString)
import Data.Text (unpack)
import IR
import Prettyprinter
import Prettyprinter.Render.Terminal (renderStrict)

str :: forall a. String -> Doc a
str = fromString

ty2doc :: IrTy -> Doc a
ty2doc IrInt = str "uint64_t"
ty2doc IrClo = str "clo"
ty2doc IrFunTy = str "fd4fun"

-- a -> ({a;})
-- Esto es una extensión de GNU C
exprstmt :: Doc a -> Doc a
exprstmt t = parens (braces (t <> semi))

decl2doc :: IrDecl -> Doc a
decl2doc (IrVal n ty t) = ty2doc ty <+> name n <> semi
decl2doc (IrFun n retTy args t) =
  let hdr =
        ty2doc retTy
          <+> name n
          <+> tupled (map (\(x, ty) -> ty2doc ty <+> name x) args)
      body = exprstmt (ir2doc t)
  in hdr
      <+> braces (nest 2 (line <> str "return" <+> body <> semi) <> line)
      <> line

fd4Main :: [IrDecl] -> Doc a
fd4Main xs =
  str "uint64_t* fd4main()"
    <+> braces (nest 2 (line <> vsep (vals2doc xs ++ [str "return 0;"])) <> line)
  where
    vals2doc :: [IrDecl] -> [Doc a]
    vals2doc [] = []
    vals2doc (IrVal n ty t : ds) = (name n <+> str "=" <+> parens (ir2doc t) <> semi) : vals2doc ds
    vals2doc (_ : ds) = vals2doc ds

name :: String -> Doc a
name n = str $ "fd4_" ++ escape n -- prefijo fd4 para evitar colision con nombres de C.

-- Convierte nombres con caracteres no válidos en C (como la comilla simple)
-- a nombres válidos.
escape :: String -> String
escape = concatMap e1
  where
    e1 :: Char -> String
    e1 c
      | c == '_' = "__"
      | isAlpha c = [c]
      | otherwise = "_" ++ show (ord c)

stmt :: Doc a -> Doc a
stmt x = parens (braces (nest 2 (line <> x <> semi) <> line))

stmts :: [Doc a] -> Doc a
stmts xs =
  parens
    $ braces
    $ foldr (\x ds -> nest 2 (line <> x <> semi) <> ds) line xs

voidptr :: Doc a
voidptr = parens (str "void *")

funcast :: Doc a
funcast = parens (str "fd4fun")

cast :: IrTy -> Doc a -> Doc a
cast ty d = parens (ty2doc ty) <> parens d

ir2doc :: Ir -> Doc a
ir2doc (IrVar n) = name n
ir2doc (IrGlobal n) = name n
ir2doc (IrCall f args ty) =
  cast
    ty
    ( parens (funcast <+> ir2doc f)
        <> tupled (map (\a -> voidptr <> ir2doc a) args)
    )
ir2doc (IrBinaryOp Add a b) = ir2doc a <+> str "+" <+> ir2doc b
ir2doc (IrBinaryOp Sub a b) = stmts [str "fd4_sub" <> tupled [ir2doc a, ir2doc b]]
ir2doc (IrLet n nty t t') =
  stmts
    [hsep [ty2doc nty, name n, str "=", ir2doc t] <> semi <> line <> ir2doc t']
ir2doc (IrIfZ c a b) =
  parens
    $ sep [ir2doc c, nest 2 (str "?" <+> ir2doc b), nest 2 (colon <+> ir2doc a)]
ir2doc (IrPrint strng t) =
  stmts
    [ str "wprintf" <> parens (str "L" <> pretty (show strng)),
      irPrintN (ir2doc t)
    ]
ir2doc (MkClosure f args) =
  str "fd4_mkclosure"
    <> tupled (name f : pretty (length args) : map ir2doc args)
ir2doc (IrAccess t ty i) = cast ty $ parens (ir2doc t) <> brackets (pretty i)
ir2doc (IrConst (N n)) = pretty n
ir2doc (IrConst _ ) = abort "TODO"

op2doc :: BinaryOp -> Doc a
op2doc Add = str "+"
op2doc Sub = str "-"

prelude :: Doc a
prelude =
  str "#include <inttypes.h>"
    <> line
    <> str "#include <wchar.h>"
    <> line
    <> str "typedef void * (*fd4fun)(void*, void*);"
    <> line
    <> str "typedef void **clo;"
    <> line
    <> str "extern void *fd4_mkclosure(void*, int, ...);"
    <> line
    <> str "extern uint64_t fd4_printn(uint64_t);"
    <> line
    <> str "extern uint64_t fd4_sub(uint64_t, uint64_t);"
    <> line

irPrintN :: Doc a -> Doc a
irPrintN x = str "fd4_printn" <> parens (exprstmt x) -- otro parens porque es una llamada a func

-- Simplemente llamar a esta función con las irDecls.
ir2C :: IrDecls -> String
ir2C (IrDecls xs) =
  unpack
    . renderStrict
    . layoutSmart defaultLayoutOptions
    $ vsep (prelude : map decl2doc xs ++ [fd4Main xs])
