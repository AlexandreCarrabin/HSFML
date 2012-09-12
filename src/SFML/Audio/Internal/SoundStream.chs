{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.SoundStream where


import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word)
import Foreign (Ptr, Storable(..), alloca, with)
import Foreign.C.Types (CFloat, CInt, CUInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

{# import SFML.Audio.Internal.SoundStatus #} (SoundStatus)
{# import SFML.Audio.Internal.Types #} (SoundStreamPtr, SoundStream(..), SoundStreamChunk, SoundStreamChunkPtr, SoundStreamCallbackData(..), withSoundStream)

{# import SFML.System.Internal.Time #} (Time, TimePtr)
{# import SFML.System.Internal.Vector3 #} (Vector3D, Vector3FPtr, fromVector3F, toVector3F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool, cIntToBool, cIntToEnum)

#include <SFML/Audio/SoundStream.h>
#include <SFML/Audio/SoundStreamWrapper.h>


type SoundStreamGetDataCallback a = a -> IO (Maybe SoundStreamChunk)
type SoundStreamSeekCallback a = Time -> a -> IO ()

createSoundStreamCallbackData :: a -> SoundStreamGetDataCallback a -> SoundStreamSeekCallback a -> IO (StablePtr (IORef (SoundStreamCallbackData a)))
createSoundStreamCallbackData d callbackGetData callbackSeek =
    newStablePtr =<< newIORef (SoundStreamCallbackData d (wrapCallbackGetData callbackGetData) (wrapCallbackSeek callbackSeek))

wrapCallbackGetData :: SoundStreamGetDataCallback a -> (SoundStreamChunkPtr -> a -> IO Bool)
wrapCallbackGetData fn = \chunk d ->
    fn d >>= \res ->
    case res of
        Nothing -> return False
        Just c  -> poke chunk c >> return True

wrapCallbackSeek :: SoundStreamSeekCallback a -> (TimePtr -> a -> IO ())
wrapCallbackSeek fn = \timePtr d ->
    peek timePtr >>= \time ->
    fn time d    


readSoundStreamCallbackData :: StablePtr (IORef (SoundStreamCallbackData a)) -> IO (SoundStreamCallbackData a)
readSoundStreamCallbackData ptr = readIORef =<< deRefStablePtr ptr

writeSoundStreamCallbackData :: StablePtr (IORef (SoundStreamCallbackData a)) -> SoundStreamCallbackData a -> IO ()
writeSoundStreamCallbackData ptr value = deRefStablePtr ptr >>= flip writeIORef value


foreign export ccall hs_sfSoundStream_getDataCallback :: SoundStreamChunkPtr -> StablePtr (IORef (SoundStreamCallbackData a)) -> IO Bool
hs_sfSoundStream_getDataCallback :: SoundStreamChunkPtr -> StablePtr (IORef (SoundStreamCallbackData a)) -> IO Bool
hs_sfSoundStream_getDataCallback chunk value = 
    readSoundStreamCallbackData value >>= \(SoundStreamCallbackData d callbackGetData _) ->
    callbackGetData chunk d

foreign export ccall hs_sfSoundStream_seekCallback :: TimePtr -> StablePtr (IORef (SoundStreamCallbackData a)) -> IO ()
hs_sfSoundStream_seekCallback :: TimePtr -> StablePtr (IORef (SoundStreamCallbackData a)) -> IO ()
hs_sfSoundStream_seekCallback time value =
    readSoundStreamCallbackData value >>= \(SoundStreamCallbackData d _ callbackSeek) ->
    callbackSeek time d


-- |Create a new `SoundStream'.
createSoundStream :: SoundStreamGetDataCallback a  -- ^Function called when the stream needs more data
                  -> SoundStreamSeekCallback a     -- ^Function called when the stream seekd
                  -> Word                          -- ^Number of channels to use (1 = mono, 2 = stereo)
                  -> Word                          -- ^Sample rate of the sound (44100 = CD Quality)
                  -> a                             -- ^Data to pass to the callback functions
                  -> IO (SoundStream a)
createSoundStream callbackGetData callbackSeek channelCount sampleRate d = do
    dataPtr <- createSoundStreamCallbackData d callbackGetData callbackSeek
    c_stream <- {# call unsafe sfSoundStream_create_wrapper #} (cIntConv channelCount) (cIntConv sampleRate) $ castStablePtrToPtr dataPtr
    streamPtr <- newForeignPtr c_stream (finalizeSoundStream c_stream dataPtr)
    return $ SoundStream streamPtr dataPtr

-- |Destroy a `SoundStream'.
finalizeSoundStream :: SoundStreamPtr -> StablePtr (IORef (SoundStreamCallbackData a)) -> IO ()
finalizeSoundStream stream callbackData = do
    {# call unsafe sfSoundStream_destroy #} stream
    freeStablePtr callbackData

-- |Start or resume playing a `SoundStream`.
-- This function starts the stream if it was stopped, resumes it
-- if it was paused, and restarts it from beginning if it was it already playing.
-- This function uses its own thread so that it doesn't block the rest of the program while the music is played.
play :: SoundStream a -> IO ()
play stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_play #} s

-- |Pause a `SoundStream` if it was playing, otherwise it has no effect.
pause :: SoundStream a -> IO ()
pause stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_pause #} s

-- |Stop playing a `SoundStream` if it was playing or paused,
-- otherwise it has no effect.
stop :: SoundStream a -> IO ()
stop stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_stop #} s

-- |Get the current status of a `SoundStream` (stopped, paused, playing).
getStatus :: SoundStream a -> IO SoundStatus
getStatus stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getStatus #} s >>= \res ->
    return $ cIntToEnum res

-- |Get the number of channels of a `SoundStream`.
getChannelCount :: SoundStream a -> IO Word
getChannelCount stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getChannelCount #} s >>= \res ->
    return $ cIntConv res

-- |Get the sample rate of a `SoundStream`.
-- The sample rate is the number of audio samples played per second.
-- The higher, the better the quality.
getSampleRate :: SoundStream a -> IO Word
getSampleRate stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getSampleRate #} s >>= \res ->
    return $ cIntConv res

-- |Set the pitch of a `SoundStream`.
-- The pitch represents the perceived fundamental frequency of a sound; 
-- thus you can make a stream more acute or grave by changing its pitch. 
-- A side effect of changing the pitch is to modify the playing speed of the stream as well.
-- The default value for the pitch is 1.
setPitch :: SoundStream a -> Float -> IO ()
setPitch stream pitch =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_setPitch #} s (cFloatConv pitch)

-- |Set the volume of a `SoundStream`.
-- The volume is a value between 0 (mute) and 100 (full volume). Default is 100.
setVolume :: SoundStream a -> Float -> IO ()
setVolume stream volume =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_setVolume #} s (cFloatConv volume)

-- |Set the 3D position of a `SoundStream` in the audio scene.
-- Only streams with one channel (mono streams) can be spatialized. Default is (0, 0, 0)
setPosition :: SoundStream a -> Vector3D Float -> IO ()
setPosition stream position =
    withSoundStream stream $ \s ->
    with (toVector3F position) $ \p ->
    {# call unsafe sfSoundStream_setPosition_wrapper #} s p

-- |Make a `SoundStream''s position relative to the listener or absolute.
-- Making a stream relative to the listener will ensure that it will always be
-- played the same way regardless the position of the listener.
-- The default value is False.
setRelativeToListener :: SoundStream a -> Bool -> IO ()
setRelativeToListener stream relative =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_setRelativeToListener #} s (cIntFromBool relative)

-- |Set the minimum distance of a `SoundStream'.
-- The minimum distance of a stream is the maximum distance at which it is heard
-- at its maximum volume. Further than the minimum distance, it will start to fade
-- out according to its attenuation factor. A value of 0 is forbidden.
-- The default value of the minimum distance is 1.
setMinDistance :: SoundStream a -> Float -> IO ()
setMinDistance stream distance =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_setMinDistance #} s (cFloatConv distance)

-- |Set the attenuation factor of a  `SoundStream'.
-- The attenuation is a multiplication factor which makes the stream
-- more or less loud according to its distance from the listener.
-- An attenuation of 0 will produce a non-attenuated stream. On the other hand,
-- an attenuation of 100 will makes the stream fade out very quickly.
-- The default value is 1.
setAttenuation :: SoundStream a -> Float -> IO ()
setAttenuation stream attenuation =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_setAttenuation #} s (cFloatConv attenuation)

-- FIXME : Remove unsafe ?
-- |Change the current playing position of a `SoundStream'.
setPlayingOffset :: SoundStream a -> Time -> IO ()
setPlayingOffset stream time =
    withSoundStream stream $ \s ->
    with time $ \t ->
    {# call unsafe sfSoundStream_setPlayingOffset_wrapper #} s t

-- FIXME : Documentation
-- |Set whether or not a `SoundStream' should loop after reaching the end.
-- If set, the stream will restart from beginning after reaching the end and so on,
-- until it is stopped or "setLoop stream False" is called. Default state is False.
setLoop :: SoundStream a -> Bool -> IO ()
setLoop stream loop =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_setLoop #} s (cIntFromBool loop)

-- |Get the pitch of a `SoundStream'
getPitch :: SoundStream a -> IO Float
getPitch stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getPitch #} s >>= \res ->
    return $ cFloatConv res

-- |Get the volume of a `SoundStream'.
getVolume :: SoundStream a -> IO Float
getVolume stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getVolume #} s >>= \res ->
    return $ cFloatConv res

-- |Get the 3D position of a `SoundStream' in the audio scene.
getPosition :: SoundStream a -> IO (Vector3D Float)
getPosition stream =
    withSoundStream stream $ \s ->
    alloca $ \position ->
    {# call unsafe sfSoundStream_getPosition_wrapper #} s position >>
    fromVector3F <$> peek position

-- |Tell whether a `SoundStream'' position is relative to the listener or is absolute.
isRelativeToListener :: SoundStream a -> IO Bool
isRelativeToListener stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_isRelativeToListener #} s >>= \res ->
    return $ cIntToBool res

-- |Get the minimum distance of a `SoundStream'.
getMinDistance :: SoundStream a -> IO Float
getMinDistance stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getMinDistance #} s >>= \res ->
    return $ cFloatConv res

-- |Get the attenuation factor of a `SoundStream'.
getAttenuation :: SoundStream a -> IO Float
getAttenuation stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getAttenuation #} s >>= \res ->
    return $ cFloatConv res

-- |Tell whether or not a `SoundStream' is in loop mode.
getLoop :: SoundStream a -> IO Bool
getLoop stream =
    withSoundStream stream $ \s ->
    {# call unsafe sfSoundStream_getLoop #} s >>= \res ->
    return $ cIntToBool res

-- |Get the current playing position of a `SoundStream'.
getPlayingOffset :: SoundStream a -> IO Time
getPlayingOffset stream =
    withSoundStream stream $ \s ->
    alloca $ \time ->
    {# call unsafe sfSoundStream_getPlayingOffset_wrapper #} s time >>
    peek time

