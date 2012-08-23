{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Thread where


import Control.Applicative ((<$>))
import Foreign (FunPtr, freeHaskellFunPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)

{# import SFML.System.Internal.Types #} (Thread(..), ThreadPtr, withThread)

#include <SFML/System/Thread.h>


-- |Create a new 'Thread' from a function and the input data of the function.
-- 
-- Note: this does /not/ run the 'Thread', use 'launch'.
createThread :: a -> (a -> IO ()) -> IO Thread
createThread a fn = newStablePtr a >>= \sp ->
    mkThreadFn fn >>= \fp ->
    c_createThread fp sp >>= \t ->
    mkThread t sp fp

mkThreadFn :: (a -> IO ()) -> IO (FunPtr (StablePtr a -> IO ()))
mkThreadFn fn = wrapThreadFn $ \sp ->
    deRefStablePtr sp >>= \a ->
    fn a

foreign import ccall "wrapper"
    wrapThreadFn :: (StablePtr a -> IO ()) -> IO (FunPtr (StablePtr a -> IO ()))

foreign import ccall unsafe "SFML/System/Thread.h sfThread_create"
    c_createThread :: FunPtr (StablePtr a -> IO ()) -> StablePtr a -> IO (ThreadPtr)

mkThread :: ThreadPtr -> StablePtr a -> FunPtr (StablePtr a -> IO ()) -> IO Thread
mkThread ptr sp fp = Thread <$> newForeignPtr ptr (finalizeThread ptr sp fp)

-- |Destroy a 'Thread'.
finalizeThread :: ThreadPtr -> StablePtr a -> FunPtr (StablePtr a -> IO ()) -> IO ()
finalizeThread t sp fp = do
    {# call unsafe sfThread_destroy #} t
    freeStablePtr sp
    freeHaskellFunPtr fp

-- |Run a 'Thread'.
-- 
-- This function starts the entry point passed to 'createThread' function, and returns immediately.
-- After this function returns, the thread's function is running in parallel to the calling code.
launch :: Thread -> IO ()
launch thread =
    withThread thread $ \ptr ->
    {# call unsafe sfThread_launch #} ptr

-- |Wait until a 'Thread' finishes.
-- 
-- Warning: if the thread function never ends, the calling thread will block forever.
-- 
-- If this function is called from its owner thread, it returns without doing anything.
wait :: Thread -> IO ()
wait thread =
    withThread thread $ \ptr ->
    {# call unsafe sfThread_wait #} ptr

-- |Terminate a 'Thread'.
-- 
-- This function immediately stops the thread, without waiting for its function to finish.
terminate :: Thread -> IO ()
terminate thread =
    withThread thread $ \ptr ->
    {# call unsafe sfThread_terminate #} ptr

