module Singleraeh.Either where

import Singleraeh.Demote
import Data.Kind ( Type )

-- | Singleton 'Either'.
type SEither :: (l -> Type) -> (r -> Type) -> Either l r -> Type
data SEither sl sr elr where
    SLeft  :: sl l -> SEither sl sr (Left  l)
    SRight :: sr r -> SEither sl sr (Right r)

demoteSEither
    :: forall dl dr sl sr elr
    .  (forall l. sl l -> dl)
    -> (forall r. sr r -> dr)
    -> SEither sl sr elr
    -> Either dl dr
demoteSEither demoteSL demoteSR = \case
  SLeft  sl -> Left  $ demoteSL sl
  SRight sr -> Right $ demoteSR sr

instance (Demotable sl, Demotable sr) => Demotable (SEither sl sr) where
    type Demote (SEither sl sr) = Either (Demote sl) (Demote sr)
    demote = demoteSEither demote demote
