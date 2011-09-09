module Text.Gogh.SafeShow where

class SafeShow a where
  -- | This function provides an Html-safe (escaped) 'String' version of the type.
  safeShow :: a -> String

instance SafeShow Bool where
  safeShow = show

instance SafeShow Char where
  safeShow c = case c of
                 '<'  -> "&lt;"
                 '>'  -> "&gt;"
                 '&'  -> "&amp;"
                 '"'  -> "&quot;"
                 '\'' -> "&#39;"
                 x    -> [x]

instance SafeShow a => SafeShow [a] where
  safeShow = concatMap safeShow

instance SafeShow Int where
  safeShow = show

instance SafeShow Integer where
  safeShow = show

instance SafeShow a => SafeShow (Maybe a)  where
  safeShow (Just x) = safeShow x
  safeShow Nothing = show (Nothing :: Maybe ())
