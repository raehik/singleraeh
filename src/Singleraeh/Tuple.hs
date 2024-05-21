module Singleraeh.Tuple where

data STuple2 sa sb ab where
    STuple2 :: sa a -> sb b -> STuple2 sa sb '(a, b)
