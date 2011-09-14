module Text.Gogh.Compiler.Parser
    ( TmplFile (..), TmplName, Tmpl (..), TmplElement (..)
    , TmplExp (..), TmplBinOp (..), TmplUnOp (..)
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
import Text.Parsec.Prim
import Text.Parsec.String

type TmplModule = String

data TmplFile = TmplFile TmplModule [Tmpl]
                deriving (Show, Eq)

type TmplName = String

data Tmpl = Tmpl { tmplName :: TmplName
                 , tmplVars :: [String]
                 , tmplElements :: [TmplElement]
                 }
            deriving (Show, Eq)

data TmplElement = TmplHtml String
                 | TmplPrint TmplExp
                 | TmplIf
                   (TmplExp, [TmplElement])   -- ^ The "if" part
                   [(TmplExp, [TmplElement])] -- ^ The various "elifs"
                   (Maybe [TmplElement])      -- ^ The "else"
                 | TmplForeach
                   String                -- ^ The variable that we bind in each cycle
                   TmplExp
                   [TmplElement]         -- ^ The main body
                 | TmplCall String [String]
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
              | TmplEmpty
                deriving (Show, Eq)

operatorTable :: OperatorTable String () Identity TmplExp
operatorTable = [ [ unary "not" TmplNot
                  , unary "is" TmplIsJust
                  , unary "isnt" TmplIsNothing
                  , unary "empty" TmplEmpty ]
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
term = spaces >> (try (fmap TmplVar (spaces >> varid))
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

element :: Parser TmplElement
element = literal <|> print' <|> ifBlock <|> foreach <|> call <|> printImpl <|> html
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
                                       ; v <- varid
                                       ; spaces >> string "in"
                                       ; e <- expr
                                       ; return (v, e)
                                       }
      elements <- many element
      closeTag foreachTag
      return $ TmplForeach v e elements

    call = do
      (name, vars) <- openTag callTag $ (,) <$> (spaces1 >> varid) <*> many (spaces1 >> varid)
      return $ TmplCall name vars
      
    html = fmap TmplHtml getHtml

template :: Parser Tmpl
template = do
  spaces
  (name, vars) <- openTag templateTag $ (,) <$> (spaces1 >> varid) <*> many (spaces1 >> varid)
  elements <- many element
  closeTag templateTag
  return $ Tmpl name vars elements

module' :: Parser TmplModule
module' = spaces >>
          openTag moduleTag (spaces >> fmap (intercalate ".") (conid `sepBy` char '.'))

parser :: Parser TmplFile
parser = do
  m <- module'
  templates <- many template
  return $ TmplFile m templates

parseTemplates :: String -> IO (Either ParseError TmplFile)
parseTemplates file = fmap (runP parser () file) $ readFile file
