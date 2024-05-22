module Singleraeh.Sing where

import Data.Kind ( Type, Constraint )
import GHC.TypeLits

class Sing (sa :: ak -> Type) where
    type SingC sa :: ak -> Constraint
    sing' :: forall (a :: ak). SingC sa a => sa a

sing
    :: forall {ak} (sa :: ak -> Type) (a :: ak)
    .  (Sing sa, SingC sa a) => sa a
sing = sing' @_ @sa @a

instance Sing SNat where
    type SingC SNat = KnownNat
    sing' = natSing

instance Sing SSymbol where
    type SingC SSymbol = KnownSymbol
    sing' = symbolSing

instance Sing SChar where
    type SingC SChar = KnownChar
    sing' = charSing
