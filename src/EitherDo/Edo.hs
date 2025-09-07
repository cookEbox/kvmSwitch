{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo       #-}

module EitherDo.Edo where

import qualified Prelude as P
import           Prelude ( Either (..)
                         , IO
                         , const
                         , either
                         , flip
                         , foldr
                         , pure
                         , (.)
                         , (<$>) 
                         )

type IOEither e a = IO (Either e a)

(>>=) :: IOEither e a -> (a -> IOEither e b) -> IOEither e b
m >>= k = do
  ea <- m
  case ea of
    Left  e -> pure (Left e)
    Right a -> k a

(>>) :: IOEither e a -> IOEither e b -> IOEither e b
m1 >> m2 = m1 >>= const m2

onRight :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
onRight io f = io P.>>= either (pure . Left) f

(|>) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
(|>) = onRight
infixr 0 |>

return :: a -> IOEither e a
return = pure . Right

ok  :: a -> IOEither e a
ok  = return

bad :: e -> IOEither e a
bad = pure . Left

sequenceE_ :: [IOEither e a] -> IOEither e ()
sequenceE_ = foldr (\m acc -> m >>= const acc) (ok ())

traverseE_ :: (x -> IOEither e a) -> [x] -> IOEither e ()
traverseE_ f xs = sequenceE_ (f <$> xs)

forE_ :: [x] -> (x -> IOEither e a) -> IOEither e ()
forE_ = flip traverseE_

