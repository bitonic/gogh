module Text.Gogh.Compiler.CodeGen.JavaScript (printTemplates) where

import Prelude hiding (elem, exp)
import Text.PrettyPrint hiding (braces)

import Text.Gogh.Compiler.Parser

printTemplates :: File -> String
printTemplates = render . file

file :: File -> Doc
file (File module' tmpls) = braces (text "(function ()")
                                   (object module' $+$ vsep (map (template module') tmpls))
                                   (text ")();")

indent :: Doc -> Doc
indent = nest 4

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

braces :: Doc -- ^ Before
       -> Doc -- ^ In the braces
       -> Doc -- ^ After
       -> Doc
braces b doc a = (b <+> lbrace) $+$ indent doc $+$ (rbrace <> a)

bracesE :: Doc -> Doc -> Doc
bracesE b doc = braces b doc empty

equalsBool :: Doc
equalsBool = text "==="

contentVar :: Doc
contentVar = text "_content"

dotted :: [Doc] -> Doc
dotted = hcat . punctuate (char '.')
          
object :: Module -> Doc
object module' = bracesE (text "if" <+>
                          parens (text "typeof" <+> objName <+> equalsBool <+> text "'undefined'"))
                         (objName <+> equals <+> text "{}" <> semi)
  where
    objName = text module'

funArgs :: [VarId] -> Doc
funArgs = parens . hsep . punctuate comma . map text

template :: Module -> Template -> Doc
template module' (Template name vars elems) = braces (dotted [text module', text name] <+> equals <+>
                                                      text "function" <+> funArgs vars)
                                                     (elementsStart module' elems)
                                                     semi

elementsStart :: Module -> [Element] -> Doc
elementsStart module' elems = text "var" <+> contentVar <+> equals <+> text "\"\"" <> semi $+$
                              elements module' elems $+$
                              text "return" <+> contentVar <> semi

elements :: Module -> [Element] -> Doc
elements module' = vsep . map (element module')

ccContent :: Doc
ccContent = contentVar <+> text "+=" <> space

element :: Module -> Element -> Doc
element _ (Html s) = ccContent <> (text . show $ s) <> semi
element _ (Print var) = ccContent <> text var <> semi
element m (If if' elifs else') = block False "if" if' $+$ elifsp $+$ elsep $+$ rbrace
  where
    block rb t (e, elems) = (if rb then rbrace else empty)
                            <+> text t <+> parens (exp e) <+> lbrace $+$ indent (elements m elems)
    elifsp = vsep . map (block True "else if") $ elifs
    elsep = case else' of
      Nothing -> empty
      Just elems -> rbrace <+> text "else" <+> lbrace $+$ indent (elements m elems)
element m (Foreach var generator elems) = dotted [text generator, text "foreach" <> parens fun]
                                          <> semi
  where
    fun = bracesE (text "function" <+> parens (text var)) (elements m elems)
element m (Call f vars) = ccContent <> dotted [text m, text f <> funArgs vars <> semi]

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
unOp IsJust e = text "notNull" <> parens (exp e)
unOp IsNothing e = text "isNull" <> parens (exp e)
unOp Not e = text "!" <> exp e
unOp Empty e = dotted [exp e, text "empty()"]
