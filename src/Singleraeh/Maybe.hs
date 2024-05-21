module Singleraeh.Maybe where

-- | Singleton 'Maybe'.
data SMaybe sa (ma :: Maybe a) where
    SJust    :: sa a -> SMaybe sa (Just a)
    SNothing ::         SMaybe sa Nothing
