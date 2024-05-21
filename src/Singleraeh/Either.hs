module Singleraeh.Either where

import Data.Kind ( Type )

-- | Singleton 'Either'.
type SEither :: (a -> Type) -> (b -> Type) -> Either a b -> Type
data SEither sa sb eab where
    SLeft  :: sa a -> SEither sa sb (Left  a)
    SRight :: sb b -> SEither sa sb (Right b)

demoteSEither
    :: forall dl dr sl sr elr
    .  (forall l. sl l -> dl)
    -> (forall r. sr r -> dr)
    -> SEither sl sr elr
    -> Either dl dr
demoteSEither demoteSL demoteSR = \case
  SLeft  sl -> Left  $ demoteSL sl
  SRight sr -> Right $ demoteSR sr
