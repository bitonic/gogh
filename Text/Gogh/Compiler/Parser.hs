module Text.Gogh.Compiler.Parser
    ( TmplFile (..), TmplName, Tmpl (..), TmplElement (..)
    , TmplExp (..), TmplBinOp (..), TmplUnOp (..)
    , parseTemplates, parser
    ) where

import Control.Applicative ((<*>), (<$>), (*>), (<*))
import Control.Monad (mzero)
import Control.Monad.Identity (Identity)
import Language.Haskell.Exts.Syntax (SrcLoc (..))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String

data TmplFile = TmplFile SrcLoc [Tmpl]
                deriving (Show, Eq)

type TmplName = String

data Tmpl = Tmpl { tmplLoc :: SrcLoc
                 , tmplName :: TmplName
                 , tmplVars :: [String]
                 , tmplElements :: [TmplElement]
                 }
            deriving (Show, Eq)

data TmplElement = TmplHtml String
                 | TmplPrint TmplExp
                 | TmplIf (TmplExp, [TmplElement]) [(TmplExp, [TmplElement])]
                   (Maybe [TmplElement])
                 | TmplForeach String TmplExp [TmplElement]
                   deriving (Show, Eq)

data TmplExp = TmplBinOp TmplBinOp TmplExp TmplExp
             | TmplUnOp TmplUnOp TmplExp
             | TmplVar String
               deriving (Show, Eq)

data TmplBinOp = TmplEq
               | TmplNotEq
               | TmplLess
               | TmplLessEq
               | TmplGreater
               | TmplGreaterEq
               | TmplAnd
               | TmplOr
                 deriving (Show, Eq)

data TmplUnOp = TmplIsJust
              | TmplIsNothing
              | TmplNot
                deriving (Show, Eq)

operatorTable :: OperatorTable String () Identity TmplExp
operatorTable = [ [ unary "not" TmplNot
                  , unary "is" TmplIsJust
                  , unary "isnt" TmplIsNothing ]
                , [ binary ">" TmplLess
                  , binary ">=" TmplLessEq
                  , binary "<" TmplGreater
                  , binary "<=" TmplGreaterEq ]
                , [ binary "==" TmplEq
                  , binary "/=" TmplNotEq ]
                , [ binary "&&" TmplAnd ]
                , [ binary "||" TmplOr ]
                ]
  where
    unary s f = Prefix (try (string s >> spaces1) >> return (TmplUnOp f))
    binary s f = Infix (try (string s) >> return (TmplBinOp f)) AssocLeft

spaces1 :: Parser ()
spaces1 = space >> spaces

reservedWords :: [String]
reservedWords = ["not", "is", "isnt", "if", "elif", "else", "foreach", "template"]

expr :: Parser TmplExp
expr = buildExpressionParser operatorTable term
       <?> "expression"

term :: Parser TmplExp
term = spaces >> (try (fmap TmplVar variable)
                  <|> between (char '(') (char ')') expr
                  <?> "simple expression")

variableChars :: String
variableChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_', '\'']

variable :: Parser String
variable = do
  spaces
  first <- oneOf $ ['a'..'z'] ++ ['A'..'Z']
  rest <- many $ oneOf variableChars
  let var = first : rest
  if var `elem` reservedWords
    then mzero
    else spaces >> return var

getSrcLoc :: Parser SrcLoc
getSrcLoc = do
  State {statePos = pos} <- getParserState
  return $ SrcLoc { srcFilename = sourceName pos
                  , srcLine = sourceLine pos
                  , srcColumn = sourceColumn pos
                  }

openTag :: String -> Parser a -> Parser a
openTag t = between (try (string ('{' : t))) (char '}' >> spaces)

-- openCloseTag :: String -> Parser a -> Parser a
-- openCloseTag t p = try (spaces *> string ('{' : t)) *> p <* string "/}" <* spaces

closeTag :: String -> Parser ()
closeTag t = string ("{/" ++ t ++ "}") >> spaces

templateTag, literalTag, printTag, ifTag, elifTag, elseTag, foreachTag :: String
templateTag = "template"
literalTag = "literal"
printTag = "print"
ifTag = "if"
elifTag = "elif"
elseTag = "else"
foreachTag = "foreach"

getHtml :: Parser String
getHtml = do
  s <- many1 $ satisfy (/= '{')
  if null s
    then mzero
    else return s

element :: Parser TmplElement
element = literal <|> print' <|> ifBlock <|> foreach <|> printImpl <|> html
  where
    literal =
      openTag literalTag (return ()) *> fmap TmplHtml getHtml <* closeTag literalTag

    print' = openTag printTag $ spaces1 >> fmap TmplPrint expr

    printImpl = try (char '{' *> fmap TmplPrint expr) <* char '}'

    ifBlock = do
      e <- openTag ifTag $ spaces1 >> expr
      elements <- many element
      elifs <- many elifBlock
      else' <- optionMaybe elseBlock
      closeTag ifTag
      return $ TmplIf  (e, elements) elifs else'

    elifBlock = do
      e <- openTag elifTag $ spaces1 >> expr
      elements <- many element
      return (e, elements)

    elseBlock = do
      openTag elseTag (return ())
      elements <- many element
      return elements

    foreach = do
      (v, e) <- openTag foreachTag $ do { spaces1
                                       ; v <- variable
                                       ; spaces >> string "in"
                                       ; e <- expr
                                       ; return (v, e)
                                       }
      elements <- many element
      closeTag foreachTag
      return $ TmplForeach v e elements

    html = fmap TmplHtml getHtml

template :: Parser Tmpl
template = do
  loc <- getSrcLoc
  spaces
  (name, vars) <- openTag templateTag $ (,) <$> variable <*> many variable
  elements <- many element
  closeTag templateTag
  return $ Tmpl loc name vars elements

parser :: Parser TmplFile
parser = fmap (uncurry TmplFile) ((,) <$> getSrcLoc <*> many template)

parseTemplates :: String -> IO (Either ParseError TmplFile)
parseTemplates file = fmap (runP parser () file) $ readFile file
