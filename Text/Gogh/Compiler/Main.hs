{-# Language TupleSections, PatternGuards #-}
module Main where

import Control.Arrow (second)
import Data.List (delete)
import System.Environment (getArgs)
import System.IO (stdout, openFile, IOMode (..), hPutStr, hClose)

import qualified Text.Gogh.Compiler.CodeGen.Haskell as HS
import qualified Text.Gogh.Compiler.CodeGen.JavaScript as JS
import Text.Gogh.Compiler.Parser

usage :: IO ()
usage = error $ "\n" ++
        "Usage: gogh --haskell infile [-o outfile]\n" ++
        "       gogh --javascript [infiles] [-o outfile] -- Minimun 1 input file"

getArg :: String                 -- ^ The argument to search for
         -> [String]            -- ^ The list of args
         -> Maybe (String, [String]) -- ^ The value and the remaining args
getArg arg args = go args
  where
    go []  = Nothing
    go [_] = Nothing
    go (a : v : as) | a == arg   = Just (v, as)
                    | otherwise = fmap (second (a :)) $ go (v : as)

getFlag :: String -> [String] -> (Bool, [String])
getFlag flag args = let args' = delete flag args
                    in (length args' /= length args, args')

main :: IO ()
main = do
  args <- getArgs
  (output, args') <- case getArg "-o" args of
                       Nothing            -> return (stdout, args)
                       Just (file, args') -> fmap (, args') $ openFile file WriteMode
  let (hs, hsargs) = getFlag "--haskell" args'
      (js, jsargs) = getFlag "--javascript" args'
      handleError (Left err) = error $ '\n' : show err
      handleError (Right s)  = return s
      haskell | [fn] <- hsargs = readFile fn >>= handleError . parseTemplates fn >>=
                                 hPutStr output . HS.printTemplates
              | otherwise      = error $ show hsargs
      javascript = mapM (\fn -> readFile fn >>= handleError . parseTemplates fn) jsargs >>=
                   hPutStr output . concatMap JS.printTemplates
  if hs then haskell else (if js then javascript else usage)
  hClose output
