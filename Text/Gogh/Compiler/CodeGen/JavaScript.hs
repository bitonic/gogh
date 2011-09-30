module Text.Gogh.Compiler.CodeGen.JavaScript (printTemplates) where

import Prelude hiding (elem, exp)
import Text.PrettyPrint hiding (braces)

import Text.Gogh.Compiler.Parser

printTemplates :: File -> String
printTemplates = render . file

------ Utility functions ------------------------------------------------------
indent :: Doc -> Doc
indent = nest 4

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

dotted :: [Doc] -> Doc
dotted = hcat . punctuate (char '.')

braces :: Doc -- ^ Before
       -> Doc -- ^ In the braces
       -> Doc -- ^ After
       -> Doc
braces b doc a = (b <+> lbrace) $+$ indent doc $+$ (rbrace <> a)

bracesE :: Doc -> Doc -> Doc
bracesE b doc = braces b doc empty

funCall :: Doc
funCall = lparen <> rparen

------ Keywords ---------------------------------------------------------------
equalsBool, functionId, undefinedId, typeofId, ifId, elifId, elseId, foreachId :: Doc
varId, returnId, goghId, notNullId, isNullId, emptyId, notId, safeShowId, appendId :: Doc
stringBuilderId, newId, toStringId :: Doc
equalsBool = text "==="
functionId = text "function"
undefinedId = text "'undefined'"
typeofId = text "typeof"
ifId = text "if"
elifId = text "else if"
elseId = text "else"
foreachId = text "foreach"
varId = text "var"
returnId = text "return"
goghId = text "Gogh"
notNullId = dotted [goghId, text "notNull"]
isNullId = dotted [goghId, text "isNull"]
emptyId = text "empty"
notId = char '!'
safeShowId = text "safeShow"
appendId = text "append"
stringBuilderId = text "StringBuilder"
newId = text "new"
toStringId = text "toString"

------ Pretty printer ---------------------------------------------------------

file :: File -> Doc
file (File module' tmpls) = object module' $+$ vsep (map (template module') tmpls)

builderVar :: Doc
builderVar = text "_builder"

object :: Module -> Doc
object module' = bracesE (ifId <+> parens (typeofId  <+> objName <+> equalsBool <+> undefinedId))
                         (objName <+> equals <+> lbrace <> rbrace <> semi)
  where
    objName = text module'

funArgs :: [VarId] -> Doc
funArgs = parens . hsep . punctuate comma . map text

template :: Module -> Template -> Doc
template module' (Template name vars elems) = braces (dotted [text module', text name] <+> equals <+>
                                                      functionId <+> funArgs vars)
                                                     (elementsStart module' elems)
                                                     semi

elementsStart :: Module -> [Element] -> Doc
elementsStart module' elems = varId <+> builderVar <+> equals <+> builder <> semi $+$
                              elements module' elems $+$
                              returnId <+> dotted [builderVar, toStringId <> funCall] <> semi
  where
    builder = newId <+> stringBuilderId <> funCall

elements :: Module -> [Element] -> Doc
elements module' = vsep . map (element module')

ccContent :: Doc -> Doc
ccContent d = dotted [builderVar, appendId <> lparen <> d <> rparen]

element :: Module -> Element -> Doc
element _ (Html s) = ccContent (text . show $ s) <> semi
element _ (Print var) = ccContent (dotted [text var, safeShowId <> funCall]) <> semi
element m (If if' elifs else') = block False ifId if' $+$ elifsp $+$ elsep $+$ rbrace
  where
    block rb t (e, elems) = (if rb then rbrace else empty)
                            <+> t <+> parens (exp e) <+> lbrace $+$ indent (elements m elems)
    elifsp = vsep . map (block True elifId) $ elifs
    elsep = case else' of
      Nothing -> empty
      Just elems -> rbrace <+> elseId <+> lbrace $+$ indent (elements m elems)
element m (Foreach var generator elems) = dotted [text generator, foreachId <> parens fun]
                                          <> semi
  where
    fun = bracesE (functionId <+> parens (text var)) (elements m elems)
element m (Call f vars) = ccContent (dotted [text m, text f <> funArgs vars]) <> semi

exp :: Exp -> Doc
exp (BinOp op e1 e2) = parens (exp e1) <+> binOp op <+> parens (exp e2)
exp (UnOp op e) = unOp op e
exp (Var v) = text v

binOp :: BinOp -> Doc
binOp Eq = equalsBool
binOp NotEq = text "!=="
binOp Less = text "<"
binOp LessEq = text "<="
binOp Greater = text ">"
binOp GreaterEq = text ">="
binOp And = text "&&"
binOp Or = text "||"

unOp :: UnOp -> Exp -> Doc
unOp IsJust e = notNullId <> parens (exp e)
unOp IsNothing e = isNullId <> parens (exp e)
unOp Not e = notId <> exp e
unOp Empty e = dotted [exp e, emptyId <> lparen <> rparen]
