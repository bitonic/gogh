module Main where

import System.Environment (getArgs)

import Text.Gogh.Compiler.CodeGen
import Text.Gogh.Compiler.Parser

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Usage: ./closure file.soy"
    else do
      res <- parseTemplates (args !! 0)
      case res of
        Left err -> putStrLn . show $ err
        Right tmpl -> putStrLn . printTemplates $ tmpl