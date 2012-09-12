{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.SoundBuffer where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, newForeignPtr_, peek, peekArray, withArrayLen)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt, CShort, CUInt, CULong)

{# import SFML.Audio.Internal.Types #} (CSFML_SoundBuffer, SoundBuffer(..), SoundBufferPtr, withSoundBuffer)

{# import SFML.System.Internal.InputStream #} (InputStream, InputStreamPtr, withInputStream)
{# import SFML.System.Internal.Time #} (Time, TimePtr)

import SFML.Utility.Foreign (cIntConv, cIntToBool, withVoidPtr)

#include <SFML/Audio/SoundBuffer.h>
#include <SFML/Audio/SoundBufferWrapper.h>


-- |Create a new `SoundBuffer' and load it from a file.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
createSoundBufferFromFile :: FilePath -> IO SoundBuffer
createSoundBufferFromFile path =
    withCString path $ \p ->
    {# call unsafe sfSoundBuffer_createFromFile #} p >>= \res ->
    mkSoundBuffer res

-- |Create a new `SoundBuffer' and load it from a file in memory.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
createSoundBufferFromMemory :: ByteString -> IO SoundBuffer
createSoundBufferFromMemory memory =
    withVoidPtr memory $ \(m, size) ->
    {# call unsafe sfSoundBuffer_createFromMemory #} m size >>= \res ->
    mkSoundBuffer res

-- FIXME: Remove unsafe ?
-- |Create a new `SoundBuffer' from a custom `InputStream'.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
createSoundBufferFromStream :: InputStream a -> IO SoundBuffer
createSoundBufferFromStream stream =
    withInputStream stream $ \s ->
    {# call sfSoundBuffer_createFromStream #} s >>= \res ->
    mkSoundBuffer res

-- |Create a new `SoundBuffer' and load it from an array of samples in memory.
-- The assumed format of the audio samples is 16 bits signed integer.
createSoundBufferFromSamples :: [Int]  -- ^List of samples
                             -> Word   -- ^Number of channels
                             -> Word   -- ^Sample rate
                             -> IO SoundBuffer
createSoundBufferFromSamples samples channelCount sampleRate =
    withArrayLen (map cIntConv samples) $ \size samples_ptr ->
    {# call unsafe sfSoundBuffer_createFromSamples #} samples_ptr (cIntConv size) (cIntConv channelCount) (cIntConv sampleRate) >>= \res ->
    mkSoundBuffer res

mkSoundBuffer :: SoundBufferPtr -> IO SoundBuffer
mkSoundBuffer ptr = SoundBuffer <$> newForeignPtr c_destroySoundBuffer ptr

mkConstSoundBuffer :: SoundBufferPtr -> IO SoundBuffer
mkConstSoundBuffer ptr = SoundBuffer <$> newForeignPtr_ ptr

-- |Create a new `SoundBuffer' by copying an existing one.
copySoundBuffer :: SoundBuffer -> IO SoundBuffer
copySoundBuffer buffer =
    withSoundBuffer buffer $ \b ->
    {# call unsafe sfSoundBuffer_copy #} b >>= \res ->
    mkSoundBuffer res

-- |Destroy a `SoundBuffer'.
foreign import ccall unsafe "SFML/Audio/SoundBuffer.h &sfSoundBuffer_destroy"
    c_destroySoundBuffer :: FinalizerPtr CSFML_SoundBuffer

-- |Save the `SoundBuffer' to a file on disk.
-- The supported audio formats are ogg, wav, flac, aiff, au, raw, paf, svx, nist,
-- voc, ircam, w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
saveToFile :: SoundBuffer -> FilePath -> IO Bool
saveToFile buffer path =
    withSoundBuffer buffer $ \b ->
    withCString path $ \p ->
    {# call unsafe sfSoundBuffer_saveToFile #} b p >>= \res ->
    return $ cIntToBool res

-- |Get the list of audio samples stored in a `SoundBuffer'.
getSamples :: SoundBuffer -> IO [Int]
getSamples buffer =
    withSoundBuffer buffer $ \b ->
    {# call unsafe sfSoundBuffer_getSampleCount #} b >>= \size ->
    {# call unsafe sfSoundBuffer_getSamples #} b >>= \samples ->
    map cIntConv <$> peekArray (cIntConv size) samples

-- |Get the number of samples stored in a `SoundBuffer'.
getSampleCount :: SoundBuffer -> IO Word
getSampleCount buffer =
    withSoundBuffer buffer $ \b ->
    {# call unsafe sfSoundBuffer_getSampleCount #} b >>= \res ->
    return $ cIntConv res

-- |Get the sample rate of a `SoundBuffer'.
getSampleRate :: SoundBuffer -> IO Word
getSampleRate buffer =
    withSoundBuffer buffer $ \b ->
    {# call unsafe sfSoundBuffer_getSampleRate #} b >>= \res ->
    return $ cIntConv res

-- |Get the number of channels used by a `SoundBuffer'.
getChannelCount :: SoundBuffer -> IO Word
getChannelCount buffer =
    withSoundBuffer buffer $ \b ->
    {# call unsafe sfSoundBuffer_getChannelCount #} b >>= \res ->
    return $ cIntConv res

-- |Get the total duration of a `SoundBuffer'.
getDuration :: SoundBuffer -> IO Time
getDuration buffer =
    withSoundBuffer buffer $ \b ->
    alloca $ \time ->
    {# call unsafe sfSoundBuffer_getDuration_wrapper #} b time >>
    peek time

