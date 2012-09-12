{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.SoundBufferRecorder where


import Control.Applicative ((<$>))
import Data.Word (Word)
import Foreign (FinalizerPtr, newForeignPtr)
import Foreign.C.Types (CUInt)

{# import SFML.Audio.Internal.SoundBuffer #} (mkConstSoundBuffer)
{# import SFML.Audio.Internal.Types #} (CSFML_SoundBufferRecorder, SoundBufferRecorder(..), SoundBufferRecorderPtr, withSoundBufferRecorder, SoundBuffer, SoundBufferPtr)

import SFML.Utility.Foreign (cIntConv)

#include <SFML/Audio/SoundBufferRecorder.h>


-- |Create a new `SoundBufferRecorder'
createSoundBufferRecorder :: IO SoundBufferRecorder
createSoundBufferRecorder =
    {# call unsafe sfSoundBufferRecorder_create #} >>= \res ->
    mkSoundBufferRecorder res

mkSoundBufferRecorder :: SoundBufferRecorderPtr -> IO SoundBufferRecorder
mkSoundBufferRecorder ptr = SoundBufferRecorder <$> newForeignPtr c_destroySoundBufferRecorder ptr

-- |Destroy a `SoundBufferRecorder'.
foreign import ccall unsafe "SFML/Audio/SoundBufferRecorder.h &sfSoundBufferRecorder_destroy"
    c_destroySoundBufferRecorder :: FinalizerPtr CSFML_SoundBufferRecorder

-- |Start the capture of a `SoundBufferRecorder'.
-- The sample rate parameter defines the number of audio samples captured per second.
-- The higher, the better the quality (for example, 44100 samples/sec is CD quality).
-- This function uses its own thread so that it doesn't block the rest of the program while the capture runs.
-- Please note that only one capture can happen at the same time.
start :: SoundBufferRecorder -> Word -> IO ()
start recorder sampleRate =
    withSoundBufferRecorder recorder $ \r ->
    {# call unsafe sfSoundBufferRecorder_start #} r (cIntConv sampleRate)

-- |Stop the capture of a `SoundBufferRecorder'.
stop :: SoundBufferRecorder -> IO ()
stop recorder =
    withSoundBufferRecorder recorder $ \r ->
    {# call unsafe sfSoundBufferRecorder_stop #} r

-- |Get the sample rate of a `SoundBufferRecorder'.
-- The sample rate parameter defines the number of audio samples captured per second.
-- The higher, the better the quality (for example, 44100 samples/sec is CD quality).
getSampleRate :: SoundBufferRecorder -> IO Word
getSampleRate recorder =
    withSoundBufferRecorder recorder $ \r ->
    {# call unsafe sfSoundBufferRecorder_getSampleRate #} r >>= \res ->
    return $ cIntConv res

-- FIXME: Return a Maybe ? Copy the SoundBuffer as it is otherwise const ?
-- |Get the `SoundBuffer' containing the captured audio data.
-- The sound buffer is valid only after the capture has ended.
getBuffer :: SoundBufferRecorder -> IO SoundBuffer
getBuffer recorder =
    withSoundBufferRecorder recorder $ \r ->
    {# call unsafe sfSoundBufferRecorder_getBuffer #} r >>= \res ->
    mkConstSoundBuffer res

