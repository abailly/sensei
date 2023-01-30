module Data.Text.Lazy.ToText where
{-- |typeclass to convert anything to text.
    apparently there is no library for this http://comments.gmane.org/gmane.comp.lang.haskell.cafe/96962
    Servant also has its' own implementation: http://haskell-servant.github.io/servant/Servant-Common-Text.html
    Strings have a similar problem, fromString but no toString : https://mail.haskell.org/pipermail/haskell-cafe/2014-February/112576.html
-}


import           Data.Text.Lazy as T

class ToText a where
  toText :: a -> T.Text
