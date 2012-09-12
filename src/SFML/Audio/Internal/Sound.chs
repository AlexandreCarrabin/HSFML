{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.Sound where


import Control.Applicative ((<$>), (<*>))
import Data.IORef (IORef, newIORef, writeIORef)
import Foreign (FinalizerPtr, alloca, newForeignPtr, nullPtr, peek, with)
import Foreign.C.Types (CFloat, CInt)

{# import SFML.Audio.Internal.SoundBuffer #} (mkConstSoundBuffer)
{# import SFML.Audio.Internal.SoundStatus #} (SoundStatus)
{# import SFML.Audio.Internal.Types #} (CSFML_Sound, Sound(..), SoundPtr, withSound, SoundBuffer, SoundBufferPtr, withSoundBuffer)

{# import SFML.System.Internal.Time #} (Time, TimePtr)
{# import SFML.System.Internal.Vector3 #} (Vector3D, Vector3FPtr, fromVector3F, toVector3F)

import SFML.Utility.Foreign (cFloatConv, cIntFromBool, cIntToBool, cIntToEnum, ptrToMaybe)

#include <SFML/Audio/Sound.h>
#include <SFML/Audio/SoundWrapper.h>


getIORefBuffer :: Sound -> IORef (Maybe SoundBuffer)
getIORefBuffer (Sound _ b) = b


-- |Create a new `Sound'.
createSound :: IO Sound
createSound =
    {# call unsafe sfSound_create #} >>= \res ->
    mkSound res Nothing

mkSound :: SoundPtr -> Maybe SoundBuffer -> IO Sound
mkSound ptr buffer = Sound <$> newForeignPtr c_destroySound ptr <*> newIORef buffer

-- |Create a new `Sound' by copying an existing one.
copySound :: Sound -> IO Sound
copySound sound =
    withSound sound $ \s ->
    {# call unsafe sfSound_copy #} s >>= \res ->
    getBuffer sound >>= \buffer ->
    mkSound res buffer

-- |Destroy a `Sound'.
foreign import ccall unsafe "SFML/Audio/Sound.h &sfSound_destroy"
    c_destroySound :: FinalizerPtr CSFML_Sound

-- |Start or resume playing a `Sound`.
-- This function starts the sound if it was stopped, resumes it
-- if it was paused, and restarts it from beginning if it was it already playing.
-- This function uses its own thread so that it doesn't block the rest of the program while the sound is played.
play :: Sound -> IO ()
play sound =
    withSound sound $ \s ->
    {# call unsafe sfSound_play #} s

-- |Pause a `Sound` if it was playing, otherwise it has no effect.
pause :: Sound -> IO ()
pause sound =
    withSound sound $ \s ->
    {# call unsafe sfSound_pause #} s

-- |Stop playing a `Sound` it if was playing or paused,
-- otherwise it has no effect.
stop :: Sound -> IO ()
stop sound =
    withSound sound $ \s ->
    {# call unsafe sfSound_stop #} s

-- |Set the `SourceBuffer' containing the audio data to play.
setBuffer :: Sound -> Maybe SoundBuffer -> IO ()
setBuffer sound buffer =
    withSound sound $ \s ->
    writeIORef (getIORefBuffer sound) buffer >>
    case buffer of
        Nothing -> {# call unsafe sfSound_setBuffer #} s nullPtr
        Just b  -> withSoundBuffer b $ \b' -> 
                   {# call unsafe sfSound_setBuffer #} s b'

--FIXME: Get value from IORef ?
-- |Get the `SoundBuffer' attached to a `Sound'.
getBuffer :: Sound -> IO (Maybe SoundBuffer)
getBuffer sound =
    withSound sound $ \s ->
    {# call unsafe sfSound_getBuffer #} s >>= \res ->
    ptrToMaybe mkConstSoundBuffer res

-- FIXME : Documentation
-- |Set whether or not a `Sound' should loop after reaching the end.
-- If set, the sound will restart from beginning after reaching the end and so on,
-- until it is stopped or "setLoop sound False" is called. Default state is False.
setLoop :: Sound -> Bool -> IO ()
setLoop sound loop =
    withSound sound $ \s ->
    {# call unsafe sfSound_setLoop #} s (cIntFromBool loop)

-- |Tell whether or not a `Sound' is in loop mode.
getLoop :: Sound -> IO Bool
getLoop sound =
    withSound sound $ \s ->
    {# call unsafe sfSound_getLoop #} s >>= \res ->
    return $ cIntToBool res

-- |Get the current status of a `Sound` (stopped, paused, playing).
getStatus :: Sound -> IO SoundStatus
getStatus sound =
    withSound sound $ \m ->
    {# call unsafe sfSound_getStatus #} m >>= \res ->
    return $ cIntToEnum res

-- |Set the pitch of a `Sound`.
-- The pitch represents the perceived fundamental frequency of a sound; 
-- thus you can make a sound more acute or grave by changing its pitch. 
-- A side effect of changing the pitch is to modify the playing speed of the sound as well.
-- The default value for the pitch is 1.
setPitch :: Sound -> Float -> IO ()
setPitch sound pitch =
    withSound sound $ \m ->
    {# call unsafe sfSound_setPitch #} m (cFloatConv pitch)

-- |Set the volume of a `Sound`.
-- The volume is a value between 0 (mute) and 100 (full volume). Default is 100.
setVolume :: Sound -> Float -> IO ()
setVolume sound volume =
    withSound sound $ \m ->
    {# call unsafe sfSound_setVolume #} m (cFloatConv volume)

-- |Set the 3D position of a `Sound` in the audio scene.
-- Only sounds with one channel (mono sounds) can be spatialized. Default is (0, 0, 0)
setPosition :: Sound -> Vector3D Float -> IO ()
setPosition sound position =
    withSound sound $ \m ->
    with (toVector3F position) $ \p ->
    {# call unsafe sfSound_setPosition_wrapper #} m p

-- |Make a `Sound''s position relative to the listener or absolute.
-- Making a sound relative to the listener will ensure that it will always be
-- played the same way regardless the position of the listener.
-- The default value is False.
setRelativeToListener :: Sound -> Bool -> IO ()
setRelativeToListener sound relative =
    withSound sound $ \m ->
    {# call unsafe sfSound_setRelativeToListener #} m (cIntFromBool relative)

-- |Set the minimum distance of a `Sound'.
-- The minimum distance of a sound is the maximum distance at which it is heard
-- at its maximum volume. Further than the minimum distance, it will start to fade
-- out according to its attenuation factor. A value of 0 is forbidden.
-- The default value of the minimum distance is 1.
setMinDistance :: Sound -> Float -> IO ()
setMinDistance sound distance =
    withSound sound $ \m ->
    {# call unsafe sfSound_setMinDistance #} m (cFloatConv distance)

-- |Set the attenuation factor of a `Sound'.
-- The attenuation is a multiplication factor which makes the sound
-- more or less loud according to its distance from the listener.
-- An attenuation of 0 will produce a non-attenuated sound. On the other hand,
-- an attenuation of 100 will makes the sound fade out very quickly.
-- The default value is 1.
setAttenuation :: Sound -> Float -> IO ()
setAttenuation sound attenuation =
    withSound sound $ \m ->
    {# call unsafe sfSound_setAttenuation #} m (cFloatConv attenuation)

-- |Change the current playing position of a `Sound'.
setPlayingOffset :: Sound -> Time -> IO ()
setPlayingOffset sound time =
    withSound sound $ \m ->
    with time $ \t ->
    {# call unsafe sfSound_setPlayingOffset_wrapper #} m t

-- |Get the pitch of a `Sound'
getPitch :: Sound -> IO Float
getPitch sound =
    withSound sound $ \m ->
    {# call unsafe sfSound_getPitch #} m >>= \res ->
    return $ cFloatConv res

-- |Get the volume of a `Sound'.
getVolume :: Sound -> IO Float
getVolume sound =
    withSound sound $ \m ->
    {# call unsafe sfSound_getVolume #} m >>= \res ->
    return $ cFloatConv res

-- |Get the 3D position of a `Sound' in the audio scene.
getPosition :: Sound -> IO (Vector3D Float)
getPosition sound =
    withSound sound $ \m ->
    alloca $ \position ->
    {# call unsafe sfSound_getPosition_wrapper #} m position >>
    fromVector3F <$> peek position

-- |Tell whether a `Sound'' position is relative to the listener or is absolute.
isRelativeToListener :: Sound -> IO Bool
isRelativeToListener sound =
    withSound sound $ \m ->
    {# call unsafe sfSound_isRelativeToListener #} m >>= \res ->
    return $ cIntToBool res

-- |Get the minimum distance of a `Sound'.
getMinDistance :: Sound -> IO Float
getMinDistance sound =
    withSound sound $ \m ->
    {# call unsafe sfSound_getMinDistance #} m >>= \res ->
    return $ cFloatConv res

-- |Get the attenuation factor of a `Sound'.
getAttenuation :: Sound -> IO Float
getAttenuation sound =
    withSound sound $ \m ->
    {# call unsafe sfSound_getAttenuation #} m >>= \res ->
    return $ cFloatConv res

-- |Get the current playing position of a `Sound'.
getPlayingOffset :: Sound -> IO Time
getPlayingOffset sound =
    withSound sound $ \m ->
    alloca $ \time ->
    {# call unsafe sfSound_getPlayingOffset_wrapper #} m time >>
    peek time

