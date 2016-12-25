module HSBits () where

import Foreign.StablePtr
    ( StablePtr, newStablePtr, deRefStablePtr, castPtrToStablePtr
    , castStablePtrToPtr )
import Foreign.C.Types (CDouble(..))

foreign export ccall "from_maybe" fromMaybePtr :: StablePtr (Maybe a) -> IO (StablePtr a)
foreign export ccall "get_double" deRefStablePtr :: StablePtr CDouble -> IO CDouble

fromMaybePtr :: StablePtr (Maybe a) -> IO (StablePtr a)
fromMaybePtr x = deRefStablePtr x >>=
    maybe (return $ castStablePtr x) newStablePtr

castStablePtr :: StablePtr a -> StablePtr b
castStablePtr = castPtrToStablePtr . castStablePtrToPtr
