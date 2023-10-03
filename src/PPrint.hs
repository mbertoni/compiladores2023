module PPrint
  ( ppTTerm,
    ppTy,
    ppSTy,
    ppName,
    ppTypeDecl,
    ppTermDecl,
  )
where

import Core
import Surf
import MonadFD4

ppName :: Name -> String
ppName = id

ppSTy :: Surf.Ty -> String
ppSTy sty = "ppSTy> " <> show sty -- render . sTy2Doc

ppTy :: Core.Ty -> String
ppTy ty = "ppTy> " <> show ty -- ppSTy . ty2STy

ppTTerm :: (MonadFD4 m) => TTerm -> m String
ppTTerm tt = return $ "ppTTerm> " <> show tt

ppTermDecl :: (MonadFD4 m) => Core.Decl TTerm -> m String
ppTermDecl decl = return $ "ppTermDecl> " <> show decl

ppTypeDecl :: (MonadFD4 m) => Core.Decl Core.Ty -> m String
ppTypeDecl decl = return $ "ppTypeDecl> " <> show decl




