{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.SoundRecorder where


import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Word (Word)
import Foreign (Ptr, peekArray)
import Foreign.C.Types (CInt, CShort, CUInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

{# import SFML.Audio.Internal.Types #} (SoundRecorderPtr, SoundRecorder(..), SoundRecorderCallbackData(..), withSoundRecorder)

import SFML.Utility.Foreign (cIntConv, cIntFromBool, cIntToBool)

#include <SFML/Audio/SoundRecorder.h>
#include <SFML/Audio/SoundRecorderWrapper.h>


type SoundRecorderStartCallback a = a -> IO Bool
type SoundRecorderProcessCallback a = [Int16] -> a -> IO Bool
type SoundRecorderStopCallback a = a -> IO ()

createSoundRecorderCallbackData :: a 
                                -> SoundRecorderStartCallback a 
                                -> SoundRecorderProcessCallback a 
                                -> SoundRecorderStopCallback a 
                                -> IO (StablePtr (IORef (SoundRecorderCallbackData a)))
createSoundRecorderCallbackData d callbackStart callbackProcess callbackStop =
    newStablePtr =<< newIORef (SoundRecorderCallbackData d (wrapCallbackStart callbackStart) (wrapCallbackProcess callbackProcess) callbackStop)

wrapCallbackStart :: SoundRecorderStartCallback a -> (a -> IO CInt)
wrapCallbackStart fn = \d ->
    cIntFromBool <$> fn d

wrapCallbackProcess :: SoundRecorderProcessCallback a -> (Ptr CShort -> CInt -> a -> IO CInt)
wrapCallbackProcess fn = \intPtr size d ->
    peekArray (cIntConv size) intPtr >>= \array ->
    cIntFromBool <$> fn (map cIntConv array) d


readSoundRecorderCallbackData :: StablePtr (IORef (SoundRecorderCallbackData a)) -> IO (SoundRecorderCallbackData a)
readSoundRecorderCallbackData ptr = readIORef =<< deRefStablePtr ptr

writeSoundRecorderCallbackData :: StablePtr (IORef (SoundRecorderCallbackData a)) -> SoundRecorderCallbackData a -> IO ()
writeSoundRecorderCallbackData ptr value = deRefStablePtr ptr >>= flip writeIORef value


foreign export ccall hs_sfSoundRecorder_startCallback :: StablePtr (IORef (SoundRecorderCallbackData a)) -> IO CInt
hs_sfSoundRecorder_startCallback :: StablePtr (IORef (SoundRecorderCallbackData a)) -> IO CInt
hs_sfSoundRecorder_startCallback value = 
    readSoundRecorderCallbackData value >>= \(SoundRecorderCallbackData d callbackStart _ _) ->
    callbackStart d

foreign export ccall hs_sfSoundRecorder_processCallback :: Ptr CShort -> CInt -> StablePtr (IORef (SoundRecorderCallbackData a)) -> IO CInt
hs_sfSoundRecorder_processCallback :: Ptr CShort -> CInt -> StablePtr (IORef (SoundRecorderCallbackData a)) -> IO CInt
hs_sfSoundRecorder_processCallback array size value =
    readSoundRecorderCallbackData value >>= \(SoundRecorderCallbackData d _ callbackProcess _) ->
    callbackProcess array size d

foreign export ccall hs_sfSoundRecorder_stopCallback :: StablePtr (IORef (SoundRecorderCallbackData a)) -> IO ()
hs_sfSoundRecorder_stopCallback :: StablePtr (IORef (SoundRecorderCallbackData a)) -> IO ()
hs_sfSoundRecorder_stopCallback value = 
    readSoundRecorderCallbackData value >>= \(SoundRecorderCallbackData d _ _ callbackStop) ->
    callbackStop d


-- |Create a new `SoundRecorder'.
createSoundRecorder :: SoundRecorderStartCallback a    -- ^Function called when a new capture starts
                    -> SoundRecorderProcessCallback a  -- ^Function called each time there's audio data to process
                    -> SoundRecorderStopCallback a     -- ^Function called when the current capture stops
                    -> a                               -- ^Data to pass to the callback functions
                    -> IO (SoundRecorder a)
createSoundRecorder callbackStart callbackProcess callbackStop d = do
    dataPtr <- createSoundRecorderCallbackData d callbackStart callbackProcess callbackStop
    c_recorder <- {# call unsafe sfSoundRecorder_create_wrapper #} $ castStablePtrToPtr dataPtr
    recorderPtr <- newForeignPtr c_recorder (finalizeSoundRecorder c_recorder dataPtr)
    return $ SoundRecorder recorderPtr dataPtr

-- |Destroy a `SoundRecorder'.
finalizeSoundRecorder :: SoundRecorderPtr -> StablePtr (IORef (SoundRecorderCallbackData a)) -> IO ()
finalizeSoundRecorder recorder callbackData = do
    {# call unsafe sfSoundRecorder_destroy #} recorder
    freeStablePtr callbackData

--FIXME: Remove unsafe ?
-- |Start the capture of a `SoundRecorder'
--The sample rate parameter defines the number of audio samples captured per second.
--The higher, the better the quality (for example, 44100 samples/sec is CD quality).
--This function uses its own thread so that it doesn't block the rest of the program while the capture runs.
--Please note that only one capture can happen at the same time.
start :: SoundRecorder a -> Word -> IO ()
start recorder sampleRate =
    withSoundRecorder recorder $ \r ->
    {# call unsafe sfSoundRecorder_start #} r (cIntConv sampleRate)

--FIXME: Remove unsafe ?
-- |Stop the capture of a `SoundRecorder'.
stop :: SoundRecorder a -> IO ()
stop recorder =
    withSoundRecorder recorder $ \r ->
    {# call unsafe sfSoundRecorder_stop #} r

-- |Get the sample rate of a `SoundRecorder'.
--The sample rate defines the number of audio samples captured per second.
--The higher, the better the quality (for example, 44100 samples/sec is CD quality).
getSampleRate :: SoundRecorder a -> IO Word
getSampleRate recorder =
    withSoundRecorder recorder $ \r ->
    {# call unsafe sfSoundRecorder_getSampleRate #} r >>= \res ->
    return $ cIntConv res

-- |Check if the system supports audio capture.
--This function should always be called before using the audio capture features.
--If it returns false, then any attempt to use sfSoundRecorder will fail.
isAvailable :: IO Bool
isAvailable =
    {# call unsafe sfSoundRecorder_isAvailable #} >>= \res ->
    return $ cIntToBool res

