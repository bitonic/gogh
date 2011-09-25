module Main where

import System.Environment (getArgs)

import qualified Text.Gogh.Compiler.CodeGen.Haskell as HS
import qualified Text.Gogh.Compiler.CodeGen.JavaScript as JS
import Text.Gogh.Compiler.Parser

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then usage
    else do
      res <- parseTemplates (args !! 1)
      case res of
        Left err -> print err
        Right tmpl -> putStrLn . printFunction (args !! 0) $ tmpl
  where
    usage = error "Usage: ./gogh (--haskell|--javascript) file.gog"
  
    printFunction "--haskell"    = HS.printTemplates
    printFunction "--javascript" = JS.printTemplates
    printFunction _              = usage
