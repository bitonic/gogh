module Text.Gogh.Compiler.CodeGen.Haskell
    ( printTemplates
    ) where

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import qualified Text.Gogh.Compiler.Parser as P

genFun :: String -> String -> Exp
genFun fun module' = Var $ Qual (ModuleName fun) (Ident module')
showFun, concatFun, emptyFun, foreachFun :: Exp
showFun = genFun "Text.Gogh.SafeShow" "safeShow"
concatFun = genFun "Data.Monoid" "mconcat"
emptyFun = genFun "Data.Monoid" "mzero"
foreachFun = genFun "Text.Gogh.Compiler.Utils" "foreach"

location :: SrcLoc
location = undefined

imports :: [ImportDecl]
imports = map mod' [ "Data.Eq"
                   , "Data.Functor"
                   , "Data.Maybe"
                   , "Data.Monoid"
                   , "Data.Ord"
                   , "Text.Gogh.Compiler.Utils"
                   , "Text.Gogh.SafeShow"
                   ]
  where
    mod' name = ImportDecl location (ModuleName name) True False Nothing Nothing Nothing

printTemplates :: P.File -> String
printTemplates = prettyPrint . genTemplates

genTemplates :: P.File -> Module
genTemplates (P.File m templates) =
  Module location (ModuleName m) [] Nothing (Just exports) imports $ map genTemplate templates
  where
    exports = map (EVar . UnQual . Ident . P.tmplName) templates

genTemplate :: P.Template -> Decl
genTemplate (P.Template name sig elements) =
  FunBind [Match location (Ident name) (genPat sig) Nothing
             (UnGuardedRhs $ genElements elements) (BDecls [])]

genPat :: [String] -> [Pat]
genPat = map (PVar . Ident)

genElements :: [P.Element] -> Exp
genElements es = App concatFun (List (map genElement es))

genUnQual :: String -> Exp
genUnQual = Var . UnQual . Ident

genElement :: P.Element -> Exp
genElement (P.Html s) = Lit $ String s
genElement (P.Print v) = App showFun (genUnQual v)
genElement (P.If (e, elements) elifs else') =
  If (genExp e) (genElements elements) (elifsExp elifs)
  where
    elseExp = case else' of
                Nothing -> emptyFun
                Just elements' -> genElements elements'
    elifsExp [] = elseExp
    elifsExp ((e', elements') : elifs') =
      genElement (P.If (e', elements') elifs' else')
genElement (P.Foreach var generator elements) =
  App concatFun (App (App foreachFun foreachLambda) (genUnQual generator))
  where
    foreachLambda = Lambda undefined [PVar (Ident var)] (genElements elements)
genElement (P.Call fun vars) = foldl ((. genUnQual) . App) (genUnQual fun) vars

genExp :: P.Exp -> Exp
genExp (P.BinOp op exp1 exp2) =
  App (App (genBinOp op) (genExp exp1)) (genExp exp2)
genExp (P.UnOp op exp') = App (genUnOp op) (genExp exp')
genExp (P.Var var) = genUnQual var

genBinOp :: P.BinOp -> Exp
genBinOp P.Eq = genFun "Data.Eq" "(==)"
genBinOp P.NotEq = genFun "Data.Eq" "(/=)"
genBinOp P.Less = genFun "Data.Ord" "(<)"
genBinOp P.LessEq = genFun "Data.Ord" "(<=)"
genBinOp P.Greater = genFun "Data.Ord" "(>)"
genBinOp P.GreaterEq = genFun "Data.Ord" "(>=)"
genBinOp P.And = genFun "Data.Bool" "(&&)"
genBinOp P.Or = genFun "Data.Bool" "(||)"

genUnOp :: P.UnOp -> Exp
genUnOp P.IsJust = genFun "Data.Maybe" "isJust"
genUnOp P.IsNothing = genFun "Data.Maybe" "isNothing"
genUnOp P.Not = genFun "Data.Bool" "not"
genUnOp P.Empty = App (genFun "Data.Eq" "(==)") (genFun "Data.Monoid" "mempty")
