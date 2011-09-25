module Text.Gogh.Compiler.CodeGen.Haskell (printTemplates) where

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)
  
import qualified Text.Gogh.Compiler.Parser as P

fun :: String -> String -> Exp
fun f module' = Var $ Qual (ModuleName f) (Ident module')

showFun, concatFun, emptyFun, foreachFun :: Exp
showFun = fun "Text.Gogh.SafeShow" "safeShow"
concatFun = fun "Data.Monoid" "mconcat"
emptyFun = fun "Data.Monoid" "mzero"
foreachFun = fun "Text.Gogh.Compiler.Utils" "foreach"

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
printTemplates = prettyPrint . templates

templates :: P.File -> Module
templates (P.File m tmpls) =
  Module location (ModuleName m) [] Nothing (Just exports) imports $ map template tmpls
  where
    exports = map (EVar . UnQual . Ident . P.tmplName) tmpls

template :: P.Template -> Decl
template (P.Template name sig elems) = FunBind [Match location (Ident name) (pat sig) Nothing
                                                (UnGuardedRhs $ elements elems) (BDecls [])]

pat :: [String] -> [Pat]
pat = map (PVar . Ident)

elements :: [P.Element] -> Exp
elements es = App concatFun (List (map element es))

unQual :: String -> Exp
unQual = Var . UnQual . Ident

element :: P.Element -> Exp
element (P.Html s) = Lit $ String s
element (P.Print v) = App showFun (unQual v)
element (P.If (e, elems) elifs else') = If (exp e) (elements elems) (elifsExp elifs)
  where
    elseExp = case else' of
                Nothing -> emptyFun
                Just elements' -> elements elements'
    elifsExp [] = elseExp
    elifsExp ((e', elements') : elifs') =
      element (P.If (e', elements') elifs' else')
element (P.Foreach var generator elems) =
  App concatFun (App (App foreachFun foreachLambda) (unQual generator))
  where
    foreachLambda = Lambda undefined [PVar (Ident var)] (elements elems)
element (P.Call f vars) = foldl ((. unQual) . App) (unQual f) vars

exp :: P.Exp -> Exp
exp (P.BinOp op exp1 exp2) =
  App (App (binOp op) (exp exp1)) (exp exp2)
exp (P.UnOp op exp') = App (unOp op) (exp exp')
exp (P.Var var) = unQual var

binOp :: P.BinOp -> Exp
binOp P.Eq = fun "Data.Eq" "(==)"
binOp P.NotEq = fun "Data.Eq" "(/=)"
binOp P.Less = fun "Data.Ord" "(<)"
binOp P.LessEq = fun "Data.Ord" "(<=)"
binOp P.Greater = fun "Data.Ord" "(>)"
binOp P.GreaterEq = fun "Data.Ord" "(>=)"
binOp P.And = fun "Data.Bool" "(&&)"
binOp P.Or = fun "Data.Bool" "(||)"

unOp :: P.UnOp -> Exp
unOp P.IsJust = fun "Data.Maybe" "isJust"
unOp P.IsNothing = fun "Data.Maybe" "isNothing"
unOp P.Not = fun "Data.Bool" "not"
unOp P.Empty = App (fun "Data.Eq" "(==)") (fun "Data.Monoid" "mempty")
