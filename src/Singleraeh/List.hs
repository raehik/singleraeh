module Singleraeh.List where

-- | Singleton list.
data SList sa a where
    SCons :: sa a -> SList sa as -> SList sa (a : as)
    SNil  ::                        SList sa '[]
