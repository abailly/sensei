module Data.Maybe.Extra where

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM d p = p >>= maybe d pure
