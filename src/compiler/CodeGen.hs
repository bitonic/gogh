module CodeGen where

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import System.FilePath (takeBaseName)

import Parser

genFun :: String -> String -> Exp
genFun fun module' = Var $ Qual (ModuleName fun) (Ident module')
showFun, concatFun, emptyFun, mapFun :: Exp
showFun = genFun "Text.Show" "show"
concatFun = genFun "Data.Monoid" "mconcat"
emptyFun = genFun "Data.Monoid" "mzero"
mapFun = genFun "Data.Functor" "fmap"

imports :: SrcLoc -> [ImportDecl]
imports loc = map mod' [ "Data.Eq"
                       , "Data.Functor"
                       , "Data.Maybe"
                       , "Data.Monoid"
                       , "Data.Ord"
                       , "Text.Show"
                       ]
  where
    mod' name = ImportDecl loc (ModuleName name) True False Nothing Nothing Nothing

printTemplates :: TmplFile -> String
printTemplates = prettyPrint . genTemplates
  where
    -- myMode = PPHsMode { classIndent = 2
    --                   , doIndent = 2
    --                   , caseIndent = 2
    --                   , letIndent = 2
    --                   , whereIndent = 2
    --                   , onsideIndent = 2
    --                   , spacing = True
    --                   , layout = PPOffsideRule
    --                   , linePragmas = False
    --                   }

genTemplates :: TmplFile -> Module
genTemplates (TmplFile loc@(SrcLoc fn _ _) templates) =
  Module loc (ModuleName (takeBaseName fn)) [] Nothing (Just exports) (imports loc) $
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
  App concatFun (App (App mapFun foreachFun) (genExp generator))
  where
    foreachFun = Lambda undefined [PVar (Ident var)] (genElements elements)

genExp :: TmplExp -> Exp
genExp (TmplBinOp op exp1 exp2) =
  App (App (genBinOp op) (genExp exp1)) (genExp exp2)
genExp (TmplUnOp op exp') = App (genUnOp op) (genExp exp')
genExp (TmplVar var) = Var . UnQual . Ident $ var

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

-------------------------------------------------------------

testSrcLoc :: SrcLoc
testSrcLoc = SrcLoc "Bar/Foo.soy" 0 0

testTmpl1 :: TmplFile
testTmpl1 = TmplFile testSrcLoc
            [Tmpl testSrcLoc "helloName" ["greetingWord", "name"]
             [TmplIf ( (TmplUnOp TmplIsNothing (TmplVar "greetingWord"))
                      , [ TmplHtml "Hello "
                        , TmplPrint (TmplVar "name")
                        ]
                      )
              []
              (Just [TmplPrint (TmplVar "greetingWord"), TmplPrint (TmplVar "name")])
             ]]

