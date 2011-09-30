module Text.Gogh.Compiler.Parser
    ( File (..), Module, Name, Template  (..), VarId
    , Element (..) , Exp (..), BinOp (..), UnOp (..)
    , parseTemplates, parser
    ) where

import Control.Applicative ((<*>), (<$>), (*>), (<*))
import Control.Monad (mzero)
import Control.Monad.Identity (Identity)
import Data.List (intercalate)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Prim hiding (Empty)
import Text.Parsec.String

type Module = String

data File = File Module [Template]
          deriving (Show, Eq)

type Name = String

data Template = Template { tmplName :: Name
                         , tmplVars :: [VarId]
                         , tmplElements :: [Element]
                         }
              deriving (Show, Eq)

type VarId = String

data Element = Html String        -- ^ Literal HTML
             | Print VarId
             | If
               (Exp, [Element])   -- ^ The "if" part
               [(Exp, [Element])] -- ^ The various "elifs"
               (Maybe [Element])  -- ^ The "else"
             | Foreach
               VarId              -- ^ The variable that we bind in each cycle
               VarId
               [Element]          -- ^ The main body
             | Call VarId [VarId]
             deriving (Show, Eq)

data Exp = BinOp BinOp Exp Exp
         | UnOp UnOp Exp
         | Var VarId
         deriving (Show, Eq)


data BinOp = Eq
           | NotEq
           | Less
           | LessEq
           | Greater
           | GreaterEq
           | And
           | Or
           deriving (Show, Eq)

data UnOp = IsJust
          | IsNothing
          | Not
          | Empty
          deriving (Show, Eq)

operatorTable :: OperatorTable String () Identity Exp
operatorTable = [ [ unary "not" Not
                  , unary "is" IsJust
                  , unary "isnt" IsNothing
                  , unary "empty" Empty ]
                , [ binary ">" Less
                  , binary ">=" LessEq
                  , binary "<" Greater
                  , binary "<=" GreaterEq ]
                , [ binary "==" Eq
                  , binary "/=" NotEq ]
                , [ binary "&&" And ]
                , [ binary "||" Or ]
                ]
  where
    unary s f = Prefix (try (string s >> spaces1) >> return (UnOp f))
    binary s f = Infix (try (string s) >> return (BinOp f)) AssocLeft

spaces1 :: Parser ()
spaces1 = space >> spaces

reservedWords :: [String]
reservedWords = ["not", "is", "isnt", "if", "elif", "else", "foreach", "template"]

expr :: Parser Exp
expr = buildExpressionParser operatorTable term
       <?> "expression"

term :: Parser Exp
term = spaces >> (try (fmap Var (spaces >> varid))
                  <|> between (char '(') (char ')') expr
                  <?> "simple expression")

range :: Char -> Char -> Parser Char
range l h = satisfy (\c -> c >= l && c <= h)

large :: Parser Char
large = range 'A' 'Z'

small :: Parser Char
small = range 'a' 'z'

symbol :: Parser Char
symbol = oneOf ['_']

cons :: Parser Char -> Parser String -> Parser String
cons pc ps = pc >>= \c -> fmap (c :) ps

varid :: Parser String
varid = do
  state <- getParserState
  res <- cons small $ many (large <|> small <|> digit <|> symbol)
  if res `elem` reservedWords
    then setParserState state >> mzero
    else return res

conid :: Parser String
conid = cons large $ many (large <|> small <|> digit <|> symbol)

openTag :: String -> Parser a -> Parser a
openTag t = between (try (string ('{' : t))) (char '}' >> spaces)

closeTag :: String -> Parser ()
closeTag t = string ("{/" ++ t ++ "}") >> spaces

templateTag, literalTag, printTag, ifTag, elifTag, elseTag, foreachTag, moduleTag :: String
callTag :: String
templateTag = "template"
literalTag = "literal"
printTag = "print"
ifTag = "if"
elifTag = "elif"
elseTag = "else"
foreachTag = "foreach"
callTag = "call"
moduleTag = "module"

getHtml :: Parser String
getHtml = do
  s <- many1 $ satisfy (/= '{')
  if null s
    then mzero
    else return s

element :: Parser Element
element = literal <|> print' <|> ifBlock <|> foreach <|> call <|> printImpl <|> html
  where
    literal =
      openTag literalTag (return ()) *> fmap Html getHtml <* closeTag literalTag

    print' = openTag printTag $ spaces1 >> fmap Print varid

    printImpl = try (char '{' *> fmap Print varid) <* char '}'

    ifBlock = do
      e <- openTag ifTag $ spaces1 >> expr
      elements <- many element
      elifs <- many elifBlock
      else' <- optionMaybe elseBlock
      closeTag ifTag
      return $ If  (e, elements) elifs else'

    elifBlock = do
      e <- openTag elifTag $ spaces1 >> expr
      elements <- many element
      return (e, elements)

    elseBlock = do
      openTag elseTag (return ())
      many element

    foreach = do
      (v, e) <- openTag foreachTag $ do { spaces1
                                        ; v <- varid
                                        ; spaces >> string "in"
                                        ; e <- spaces >> varid
                                        ; return (v, e)
                                        }
      elements <- many element
      closeTag foreachTag
      return $ Foreach v e elements

    call = do
      (name, vars) <- openTag callTag $ (,) <$> (spaces1 >> varid) <*> many (spaces1 >> varid)
      return $ Call name vars

    html = fmap Html getHtml

template :: Parser Template
template = do
  spaces
  (name, vars) <- openTag templateTag $ (,) <$> (spaces1 >> varid) <*> many (spaces1 >> varid)
  elements <- many element
  closeTag templateTag
  return $ Template name vars elements

module' :: Parser Module
module' = spaces >>
          openTag moduleTag (spaces >> fmap (intercalate ".") (conid `sepBy` char '.'))

parser :: Parser File
parser = do
  m <- module'
  templates <- many template
  return $ File m templates

parseTemplates :: String -> String -> Either ParseError File
parseTemplates fn code = runP parser () fn code
