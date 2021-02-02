module MonoidSyntax (fail, (>>), (>>=)) where

import Prelude hiding (fail, (>>), (>>=))

fail :: Monoid w => String -> w
fail = error

(>>) :: Monoid w => w -> w -> w
(>>) = (<>)

(>>=) :: Monoid w => w -> (w -> w) -> w
x >>= f = x <> f x
