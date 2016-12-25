module Foo where

import Foreign.C.Types (CDouble(..))
import Foreign.StablePtr (StablePtr, newStablePtr)

foreign export ccall "hs_sqrt" hs_sqrt :: CDouble -> IO (StablePtr (Maybe CDouble))


hs_sqrt :: CDouble -> IO (StablePtr (Maybe CDouble))
hs_sqrt = newStablePtr . safeSqrt

safeSqrt :: (Ord a, Floating a) => a -> Maybe a
safeSqrt x
    | x < 0     = Nothing
    | otherwise = Just $ sqrt x
