module Text.Gogh.Compiler.Utils where

import Data.Foldable (Foldable, foldr)
import Prelude hiding (foldr)

import Text.Gogh.SafeShow

foreach :: (Foldable t, SafeShow b) => (a -> b) -> t a -> [b]
foreach f t = foldr ((:) . f) [] t
