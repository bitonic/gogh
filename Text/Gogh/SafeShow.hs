{-# Language OverloadedStrings#-}
module Text.Gogh.SafeShow (SafeShow (..)) where

import Data.Text (Text)
import qualified Data.Text as T

class SafeShow a where
  -- | This function provides an Html-safe (escaped) 'Text' version of the type.
  safeShow :: a -> Text

instance SafeShow Bool where
  safeShow = T.pack . show

instance SafeShow Char where
  safeShow c = case c of
                 '<'  -> "&lt;"
                 '>'  -> "&gt;"
                 '&'  -> "&amp;"
                 '"'  -> "&quot;"
                 '\'' -> "&#39;"
                 x    -> T.singleton x

instance SafeShow Text where
  safeShow = T.concatMap safeShow

instance SafeShow a => SafeShow [a] where
  safeShow = T.concat . map safeShow

instance SafeShow Int where
  safeShow = T.pack . show

instance SafeShow Integer where
  safeShow = T.pack . show

instance SafeShow a => SafeShow (Maybe a)  where
  safeShow (Just x) = safeShow x
  safeShow Nothing = T.pack $ show (Nothing :: Maybe ())
