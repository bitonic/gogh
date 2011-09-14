module Text.Gogh.Compiler.CodeGen.Haskell
    ( printTemplates
    ) where

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Text.Gogh.Compiler.Parser

genFun :: String -> String -> Exp
genFun fun module' = Var $ Qual (ModuleName fun) (Ident module')
showFun, concatFun, emptyFun, foreachFun :: Exp
showFun = genFun "Text.Gogh.SafeShow" "safeShow"
concatFun = genFun "Data.Monoid" "mconcat"
emptyFun = genFun "Data.Monoid" "mzero"
foreachFun = genFun "Text.Gogh.Compiler.Utils" "foreach"

imports :: SrcLoc -> [ImportDecl]
imports loc = map mod' [ "Data.Eq"
                       , "Data.Functor"
                       , "Data.Maybe"
                       , "Data.Monoid"
                       , "Data.Ord"
                       , "Text.Gogh.Compiler.Utils"
                       , "Text.Gogh.SafeShow"
                       ]
  where
    mod' name = ImportDecl loc (ModuleName name) True False Nothing Nothing Nothing

printTemplates :: TmplFile -> String
printTemplates = prettyPrint . genTemplates

genTemplates :: TmplFile -> Module
genTemplates (TmplFile loc m templates) =
  Module loc (ModuleName m) [] Nothing (Just exports) (imports loc) $
    map genTemplate templates
  where
    exports = map (EVar . UnQual . Ident . tmplName) templates

genTemplate :: Tmpl -> Decl
genTemplate (Tmpl loc name sig elements) =
  FunBind [Match loc (Ident name) (genPat sig) Nothing
             (UnGuardedRhs $ genElements elements) (BDecls [])]

genPat :: [String] -> [Pat]
genPat = map (PVar . Ident)

genElements :: [TmplElement] -> Exp
genElements es = App concatFun (List (map genElement es))

genUnQual :: String -> Exp
genUnQual = Var . UnQual . Ident

genElement :: TmplElement -> Exp
genElement (TmplHtml s) = Lit $ String s
genElement (TmplPrint e) = App showFun (genExp e)
genElement (TmplIf (e, elements) elifs else') =
  If (genExp e) (genElements elements) (elifsExp elifs)
  where
    elseExp = case else' of
                Nothing -> emptyFun
                Just elements' -> genElements elements'
    elifsExp [] = elseExp
    elifsExp ((e', elements') : elifs') =
      genElement (TmplIf (e', elements') elifs' else')
genElement (TmplForeach var generator elements) =
  App concatFun (App (App foreachFun foreachLambda) (genExp generator))
  where
    foreachLambda = Lambda undefined [PVar (Ident var)] (genElements elements)
genElement (TmplCall fun vars) = foldl ((. genUnQual) . App) (genUnQual fun) vars

genExp :: TmplExp -> Exp
genExp (TmplBinOp op exp1 exp2) =
  App (App (genBinOp op) (genExp exp1)) (genExp exp2)
genExp (TmplUnOp op exp') = App (genUnOp op) (genExp exp')
genExp (TmplVar var) = genUnQual var

genBinOp :: TmplBinOp -> Exp
genBinOp TmplEq = genFun "Data.Eq" "(==)"
genBinOp TmplNotEq = genFun "Data.Eq" "(/=)"
genBinOp TmplLess = genFun "Data.Ord" "(<)"
genBinOp TmplLessEq = genFun "Data.Ord" "(<=)"
genBinOp TmplGreater = genFun "Data.Ord" "(>)"
genBinOp TmplGreaterEq = genFun "Data.Ord" "(>=)"
genBinOp TmplAnd = genFun "Data.Bool" "(&&)"
genBinOp TmplOr = genFun "Data.Bool" "(||)"

genUnOp :: TmplUnOp -> Exp
genUnOp TmplIsJust = genFun "Data.Maybe" "isJust"
genUnOp TmplIsNothing = genFun "Data.Maybe" "isNothing"
genUnOp TmplNot = genFun "Data.Bool" "not"
genUnOp TmplEmpty = App (genFun "Data.Eq" "(==)") (genFun "Data.Monoid" "mempty")