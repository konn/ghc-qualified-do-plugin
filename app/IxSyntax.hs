module IxSyntax where

import Prelude hiding (fail, (<*>), (>>), (>>=))

fail :: IxMonad m => String -> m i j a
fail = error

class IxFunctor m where
  imap :: (a -> b) -> m i j a -> m i j a

class IxFunctor m => IxPointed m where
  ipure :: a -> m i i a

class IxPointed m => IxApplicative m where
  (<*>) :: m i j (a -> b) -> m j k a -> m i k b

class IxApplicative m => IxMonad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  a >> b = a >>= const b

ijoin :: IxMonad m => m i j (m j k a) -> m i k a
ijoin = (>>= id)