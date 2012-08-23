{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Mutex where


import Control.Applicative ((<$>))
import Foreign (FinalizerPtr, newForeignPtr)

{# import SFML.System.Internal.Types #} (CSFML_Mutex, Mutex(..), MutexPtr, withMutex)

#include <SFML/System/Mutex.h>


-- |Create a new 'Mutex'.
createMutex :: IO Mutex
createMutex =
    {# call unsafe sfMutex_create #} >>= \res ->
    mkMutex res

mkMutex :: MutexPtr -> IO Mutex
mkMutex ptr = Mutex <$> newForeignPtr c_destroyMutex ptr

-- |Destroy a 'Mutex'.
foreign import ccall unsafe "SFML/System/Mutex.h &sfMutex_destroy"
    c_destroyMutex :: FinalizerPtr CSFML_Mutex

-- |Lock a 'Mutex'.
lock :: Mutex -> IO ()
lock mutex =
    withMutex mutex $ \ptr ->
    {# call unsafe sfMutex_lock #} ptr

-- |Unlock a 'Mutex'.
unlock :: Mutex -> IO ()
unlock mutex =
    withMutex mutex $ \ptr ->
    {# call unsafe sfMutex_unlock #} ptr

