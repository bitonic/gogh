module Text.Gogh.Compiler.CodeGen.JavaScript
       ( printTemplates
       ) where

import Prelude hiding (elem, exp)
import Text.PrettyPrint hiding (braces)

import Text.Gogh.Compiler.Parser

printTemplates :: File -> String
printTemplates = render . file

file :: File -> Doc
file (File module' tmpls) = text "(function () {" $+$
                            nest 2 (object module' $+$ foldr ($+$) empty (map (template module') tmpls)) $+$
                            text "})();"

braces :: Doc -> Doc
braces doc = lbrace <+> doc <+> rbrace

equalsBool :: Doc
equalsBool = text "==="

dataVar, contentVar :: Doc
dataVar = text "data"
contentVar = text "content"

dotted :: [Doc] -> Doc
dotted = hcat . punctuate (char '.')
          
object :: Module -> Doc
object module' = text "if" <+>
                 parens (text "typeof" <+> objName <+> equalsBool <+> text "'undefined'") <+>
                 braces (objName <+> equals <+> text "{}" <> semi)
  where
    objName = text module'

template :: Module -> Template -> Doc
template module' (Template name _ elems) = dotted [text module', text name] <+> equals <+>
                                           text "function" <+> parens dataVar <+> (braces . elementsStart $ elems)

elementsStart :: [Element] -> Doc
elementsStart elems = text "var" <+> contentVar <+> equals <+> text "\"\"" <> semi <+>
                      elements elems <+>
                      text "return" <+> contentVar <> semi

elements :: [Element] -> Doc
elements = hsep . map element

ccContent :: Doc
ccContent = contentVar <+> text "+=" <> space

variable :: VarId -> Doc
variable var = dotted [dataVar, text var]

element :: Element -> Doc
element (Html s) = ccContent <> (text . show $ s) <> semi
element (Print var) = ccContent <> variable var <> semi
element (If (e', elems') elifs else') = block "if" e' elems' <+> elifsp <+> elsep
  where
    block t e elems = text t <+> parens (exp e) <+> braces (elements elems)
    elifsp = hcat . punctuate space . map (uncurry (block "else if")) $ elifs
    elsep = case else' of
      Nothing -> empty
      Just elems -> text "else" <+> braces (elements elems)
element (Foreach var generator elems) = dotted [text generator, text "foreach" <> parens fun]
                                        <> semi
  where
    fun = text "function" <+> parens (text var) <+> braces (elements elems)
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
