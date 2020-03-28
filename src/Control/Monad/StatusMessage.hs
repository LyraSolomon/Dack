module Control.Monad.StatusMessage (StatusMessage(..), fromWriter, fromEither, fromMaybe, warn, fatal) where

import Control.Monad.Writer.Lazy

-- |Stores warning and error messages involved in generating a piece of data. In the event of an error, the data is not available.
newtype StatusMessage a = Status (String, Maybe a) deriving (Show, Eq)

instance Functor StatusMessage where
  fmap f (Status (e, x)) = Status (e, fmap f x)

instance Applicative StatusMessage where
  _ <*> Status (e, Nothing)                  = Status (e, Nothing)
  Status (e1, Nothing) <*> Status (e, _)     = Status (e <> e1, Nothing)
  Status (e1, Just f) <*> Status (e, Just x) = Status (e <> e1, Just (f x))
  pure x                                     = Status (mempty, Just x)

instance Monad StatusMessage where
  Status (e, Nothing) >>= _ = Status (e, Nothing)
  Status (e, Just x)  >>= f = (\(Status (a, b)) -> Status (e <> a, b)) (f x)
  return x                  = Status (mempty, Just x)
  fail e                    = Status (e, Nothing)

-- |Appends to the warning and error messages. Data remains unchanged.
warn :: String -> StatusMessage a -> StatusMessage a
warn e1 (Status (e, x)) = Status (e <> e1, x)

-- |Appends to the warning and error messages. Removes data.
fatal :: String -> StatusMessage a -> StatusMessage a
fatal e1 (Status (e, _)) = Status (e <> e1, Nothing)

-- |Converts by using the string from the writer as the warning/error list
fromWriter :: Writer String a -> StatusMessage a
fromWriter a = let (x, e) = runWriter a in Status (e, Just x)

-- |Converts by using the string as the error message if no data is present, and using no warning if the data is present.
fromEither :: Either String a -> StatusMessage a
fromEither (Left e) = Status (e, Nothing)
fromEither (Right a) = Status (mempty, Just a)

-- |Converts by not using any warning/error message, even if there is no data
fromMaybe :: Maybe a -> StatusMessage a
fromMaybe a = Status (mempty, a)

-- |'StatusMessage'-specific version of 'sequence' where all warnings/error messages are used rather than stopping at the first error
sequenceStatus :: [StatusMessage a] -> StatusMessage [a]
sequenceStatus = foldr (\(Status (e1, x1)) (Status (e, x)) -> Status (e <> e1, (:) <$> x1 <*> x)) (return [])
