module Singleraeh.Maybe where

import Data.Kind ( Type )
import Singleraeh.Demote

-- | Singleton 'Maybe'.
type SMaybe :: (a -> Type) -> Maybe a -> Type
data SMaybe sa ma where
    SJust    :: sa a -> SMaybe sa (Just a)
    SNothing ::         SMaybe sa Nothing

demoteSMaybe
    :: forall da sa ma
    .  (forall a. sa a -> da)
    -> SMaybe sa ma
    -> Maybe da
demoteSMaybe demoteSA = \case
  SJust sa -> Just $ demoteSA sa
  SNothing -> Nothing

instance Demotable sa => Demotable (SMaybe sa) where
    type Demote (SMaybe sa) = Maybe (Demote sa)
    demote = demoteSMaybe demote
