{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.Music where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, peek, with)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CFloat, CInt, CUInt, CULong)

{# import SFML.Audio.Internal.SoundStatus #} (SoundStatus)
{# import SFML.Audio.Internal.Types #} (CSFML_Music, Music(..), MusicPtr, withMusic)

{# import SFML.System.Internal.InputStream #} (InputStream, InputStreamPtr, withInputStream)
{# import SFML.System.Internal.Time #} (Time, TimePtr)
{# import SFML.System.Internal.Vector3 #} (Vector3D, Vector3FPtr, fromVector3F, toVector3F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool, cIntToBool, cIntToEnum, withVoidPtr)

#include <SFML/Audio/Music.h>
#include <SFML/Audio/MusicWrapper.h>


-- |Create a new `Music' and load it from a file.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
createMusicFromFile :: FilePath -> IO Music
createMusicFromFile path =
    withCString path $ \p ->
    {# call unsafe sfMusic_createFromFile #} p >>= \res ->
    mkMusic res

-- |Create a new `Music' and load it from a file in memory.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
createMusicFromMemory :: ByteString -> IO Music
createMusicFromMemory memory =
    withVoidPtr memory $ \(m, size) ->
    {# call unsafe sfMusic_createFromMemory #} m size >>= \res ->
    mkMusic res

-- FIXME: Remove unsafe ?
-- |Create a new `Music' from a custom `InputStream'.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
createMusicFromStream :: InputStream a -> IO Music
createMusicFromStream stream =
    withInputStream stream $ \s ->
    {# call sfMusic_createFromStream #} s >>= \res ->
    mkMusic res

mkMusic :: MusicPtr -> IO Music
mkMusic ptr = Music <$> newForeignPtr c_destroyMusic ptr

-- |Destroy a `Music'.
foreign import ccall unsafe "SFML/Audio/Music.h &sfMusic_destroy"
    c_destroyMusic :: FinalizerPtr CSFML_Music

-- FIXME : Documentation
-- |Set whether or not a `Music' should loop after reaching the end.
-- If set, the music will restart from beginning after reaching the end and so on,
-- until it is stopped or "setLoop music False" is called. Default state is False.
setLoop :: Music -> Bool -> IO ()
setLoop music loop =
    withMusic music $ \m ->
    {# call unsafe sfMusic_setLoop #} m (cIntFromBool loop)

-- |Tell whether or not a `Music' is in loop mode.
getLoop :: Music -> IO Bool
getLoop music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getLoop #} m >>= \res ->
    return $ cIntToBool res

-- |Get the total duration of a `Music'.
getDuration :: Music -> IO Time
getDuration music =
    withMusic music $ \m ->
    alloca $ \time ->
    {# call unsafe sfMusic_getDuration_wrapper #} m time >>
    peek time

-- |Start or resume playing a `Music`.
-- This function starts the music if it was stopped, resumes it
-- if it was paused, and restarts it from beginning if it was it already playing.
-- This function uses its own thread so that it doesn't block the rest of the program while the music is played.
play :: Music -> IO ()
play music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_play #} m

-- |Pause a `Music` if it was playing, otherwise it has no effect.
pause :: Music -> IO ()
pause music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_pause #} m

-- |Stop playing a `Music` it if was playing or paused,
-- otherwise it has no effect.
stop :: Music -> IO ()
stop music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_stop #} m

-- |Get the number of channels of a `Music`.
getChannelCount :: Music -> IO Word
getChannelCount music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getChannelCount #} m >>= \res ->
    return $ cIntConv res

-- |Get the sample rate of a `Music`.
-- The sample rate is the number of audio samples played per second.
-- The higher, the better the quality.
getSampleRate :: Music -> IO Word
getSampleRate music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getSampleRate #} m >>= \res ->
    return $ cIntConv res

-- |Get the current status of a `Music` (stopped, paused, playing).
getStatus :: Music -> IO SoundStatus
getStatus music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getStatus #} m >>= \res ->
    return $ cIntToEnum res

-- |Get the current playing position of a `Music'.
getPlayingOffset :: Music -> IO Time
getPlayingOffset music =
    withMusic music $ \m ->
    alloca $ \time ->
    {# call unsafe sfMusic_getPlayingOffset_wrapper #} m time >>
    peek time

-- |Set the pitch of a `Music`.
-- The pitch represents the perceived fundamental frequency of a sound; 
-- thus you can make a music more acute or grave by changing its pitch. 
-- A side effect of changing the pitch is to modify the playing speed of the music as well.
-- The default value for the pitch is 1.
setPitch :: Music -> Float -> IO ()
setPitch music pitch =
    withMusic music $ \m ->
    {# call unsafe sfMusic_setPitch #} m (cFloatConv pitch)

-- |Set the volume of a `Music`.
-- The volume is a value between 0 (mute) and 100 (full volume). Default is 100.
setVolume :: Music -> Float -> IO ()
setVolume music volume =
    withMusic music $ \m ->
    {# call unsafe sfMusic_setVolume #} m (cFloatConv volume)

-- |Set the 3D position of a `Music` in the audio scene.
-- Only musics with one channel (mono musics) can be spatialized. Default is (0, 0, 0)
setPosition :: Music -> Vector3D Float -> IO ()
setPosition music position =
    withMusic music $ \m ->
    with (toVector3F position) $ \p ->
    {# call unsafe sfMusic_setPosition_wrapper #} m p

-- |Make a `Music''s position relative to the listener or absolute.
-- Making a music relative to the listener will ensure that it will always be
-- played the same way regardless the position of the listener.
-- The default value is False.
setRelativeToListener :: Music -> Bool -> IO ()
setRelativeToListener music relative =
    withMusic music $ \m ->
    {# call unsafe sfMusic_setRelativeToListener #} m (cIntFromBool relative)

-- |Set the minimum distance of a `Music'.
-- The minimum distance of a music is the maximum distance at which it is heard
-- at its maximum volume. Further than the minimum distance, it will start to fade
-- out according to its attenuation factor. A value of 0 is forbidden.
-- The default value of the minimum distance is 1.
setMinDistance :: Music -> Float -> IO ()
setMinDistance music distance =
    withMusic music $ \m ->
    {# call unsafe sfMusic_setMinDistance #} m (cFloatConv distance)

-- |Set the attenuation factor of a `Music'.
-- The attenuation is a multiplication factor which makes the music
-- more or less loud according to its distance from the listener.
-- An attenuation of 0 will produce a non-attenuated music. On the other hand,
-- an attenuation of 100 will makes the music fade out very quickly.
-- The default value is 1.
setAttenuation :: Music -> Float -> IO ()
setAttenuation music attenuation =
    withMusic music $ \m ->
    {# call unsafe sfMusic_setAttenuation #} m (cFloatConv attenuation)

-- FIXME : Remove unsafe ?
-- |Change the current playing position of a `Music'.
setPlayingOffset :: Music -> Time -> IO ()
setPlayingOffset music time =
    withMusic music $ \m ->
    with time $ \t ->
    {# call unsafe sfMusic_setPlayingOffset_wrapper #} m t

-- |Get the pitch of a `Music'
getPitch :: Music -> IO Float
getPitch music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getPitch #} m >>= \res ->
    return $ cFloatConv res

-- |Get the volume of a `Music'.
getVolume :: Music -> IO Float
getVolume music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getVolume #} m >>= \res ->
    return $ cFloatConv res

-- |Get the 3D position of a `Music' in the audio scene.
getPosition :: Music -> IO (Vector3D Float)
getPosition music =
    withMusic music $ \m ->
    alloca $ \position ->
    {# call unsafe sfMusic_getPosition_wrapper #} m position >>
    fromVector3F <$> peek position

-- |Tell whether a `Music'' position is relative to the listener or is absolute.
isRelativeToListener :: Music -> IO Bool
isRelativeToListener music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_isRelativeToListener #} m >>= \res ->
    return $ cIntToBool res

-- |Get the minimum distance of a `Music'.
getMinDistance :: Music -> IO Float
getMinDistance music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getMinDistance #} m >>= \res ->
    return $ cFloatConv res

-- |Get the attenuation factor of a `Music'.
getAttenuation :: Music -> IO Float
getAttenuation music =
    withMusic music $ \m ->
    {# call unsafe sfMusic_getAttenuation #} m >>= \res ->
    return $ cFloatConv res

