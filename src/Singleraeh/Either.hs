module Singleraeh.Either where

import Data.Kind ( Type )

-- | Singleton 'Either'.
type SEither :: (a -> Type) -> (b -> Type) -> Either a b -> Type
data SEither sa sb eab where
    SLeft  :: sa a -> SEither sa sb (Left  a)
    SRight :: sb b -> SEither sa sb (Right b)
