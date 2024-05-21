module Singleraeh.Tuple where

import Singleraeh.Demote
import Data.Kind ( Type )

type STuple2 :: (a -> Type) -> (b -> Type) -> (a, b) -> Type
data STuple2 sa sb ab where
    STuple2 :: sa a -> sb b -> STuple2 sa sb '(a, b)

demoteSTuple2
    :: forall da db sa sb ab
    .  (forall a. sa a -> da)
    -> (forall b. sb b -> db)
    -> STuple2 sa sb ab
    -> (da, db)
demoteSTuple2 demoteSA demoteSB (STuple2 sa sb) = (demoteSA sa, demoteSB sb)

instance (Demotable sa, Demotable sb) => Demotable (STuple2 sa sb) where
    type Demote (STuple2 sa sb) = (Demote sa, Demote sb)
    demote = demoteSTuple2 demote demote
