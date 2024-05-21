module Singleraeh.List where

import Data.Kind ( Type )

-- | Singleton list.
type SList :: (a -> Type) -> [a] -> Type
data SList sa as where
    SCons :: sa a -> SList sa as -> SList sa (a : as)
    SNil  ::                        SList sa '[]

demoteSList
    :: forall da sa as
    .  (forall a. sa a -> da)
    -> SList sa as
    -> [da]
demoteSList demoteSA = \case
  SCons sa sas -> demoteSA sa : demoteSList demoteSA sas
  SNil         -> []
