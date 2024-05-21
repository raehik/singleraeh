module Singleraeh.Tuple where

import Data.Kind ( Type )

type STuple2 :: (a -> Type) -> (b -> Type) -> (a, b) -> Type
data STuple2 sa sb ab where
    STuple2 :: sa a -> sb b -> STuple2 sa sb '(a, b)
