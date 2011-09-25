module Text.Gogh.Compiler.CodeGen.JavaScript (printTemplates) where

import Prelude hiding (elem, exp)
import Text.PrettyPrint hiding (braces)

import Text.Gogh.Compiler.Parser

printTemplates :: File -> String
printTemplates = render . file

file :: File -> Doc
file (File module' tmpls) = braces (text "(function ()")
                                   (object module' $+$ foldr (($+$) . template module') empty tmpls)
                                   (text ")();")

indent :: Doc -> Doc
indent = nest 4

braces :: Doc -- ^ Before
       -> Doc -- ^ In the braces
       -> Doc -- ^ After
       -> Doc
braces b doc a = (b <+> lbrace) $+$ indent doc $+$ (rbrace <+> a)

bracesE :: Doc -> Doc -> Doc
bracesE b doc = braces b doc empty

equalsBool :: Doc
equalsBool = text "==="

dataVar, contentVar :: Doc
dataVar = text "data"
contentVar = text "content"

dotted :: [Doc] -> Doc
dotted = hcat . punctuate (char '.')
          
object :: Module -> Doc
object module' = bracesE (text "if" <+>
                          parens (text "typeof" <+> objName <+> equalsBool <+> text "'undefined'"))
                         (objName <+> equals <+> text "{}" <> semi)
  where
    objName = text module'

template :: Module -> Template -> Doc
template module' (Template name _ elems) = bracesE (dotted [text module', text name] <+> equals <+>
                                                    text "function" <+> parens dataVar)
                                                   (elementsStart elems)

elementsStart :: [Element] -> Doc
elementsStart elems = text "var" <+> contentVar <+> equals <+> text "\"\"" <> semi $+$
                      elements elems $+$
                      text "return" <+> contentVar <> semi

elements :: [Element] -> Doc
elements = foldr (($+$) . element) empty

ccContent :: Doc
ccContent = contentVar <+> text "+=" <> space

variable :: VarId -> Doc
variable var = dotted [dataVar, text var]

element :: Element -> Doc
element (Html s) = ccContent <> (text . show $ s) <> semi
element (Print var) = ccContent <> variable var <> semi
element (If if' elifs else') = block False "if" if' $+$ elifsp $+$ elsep $+$ rbrace
  where
    block rb t (e, elems) = (if rb then rbrace else empty)
                            <+> text t <+> parens (exp e) <+> lbrace $+$ indent (elements elems)
    elifsp = foldr ($+$) empty . map (block True "else if") $ elifs
    elsep = case else' of
      Nothing -> empty
      Just elems -> rbrace <+> text "else" <+> lbrace $+$ indent (elements elems)
element (Foreach var generator elems) = dotted [text generator, text "foreach" <> parens fun]
                                        <> semi
  where
    fun = bracesE (text "function" <+> parens (text var)) (elements elems)
element (Call f _) = ccContent <> text f <> parens dataVar <> semi

exp :: Exp -> Doc
exp (BinOp op e1 e2) = parens (exp e1) <+> binOp op <+> parens (exp e2)
exp (UnOp op e) = unOp op e
exp (Var v) = variable v

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
