module Singleraeh.Maybe where

-- | Singleton 'Maybe'.
data SMaybe sa (ma :: Maybe a) where
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
