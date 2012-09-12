{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.Types where


import Control.Applicative ((<$>))
import Data.Int (Int16)
import Data.IORef (IORef)
import Foreign (ForeignPtr, Ptr, Storable(..), peekArray, plusPtr, pokeArray, withForeignPtr)
import Foreign.C.Types (CInt, CShort, CUInt)
import Foreign.StablePtr (StablePtr)

import SFML.System.Internal.Time (TimePtr)

import SFML.Utility.Foreign (cIntConv)

#include <SFML/Audio/SoundStream.h>
#include <SFML/Audio/Types.h>


type CSFML_Music = ()
{# pointer *sfMusic as MusicPtr -> CSFML_Music #}
data Music = Music (ForeignPtr CSFML_Music)

withMusic :: Music -> (MusicPtr -> IO a) -> IO a
withMusic (Music m) = withForeignPtr m


type CSFML_Sound = ()
{# pointer *sfSound as SoundPtr -> CSFML_Sound #}
data Sound = Sound (ForeignPtr CSFML_Sound)
                   (IORef (Maybe SoundBuffer))

withSound :: Sound -> (SoundPtr -> IO a) -> IO a
withSound (Sound s _) = withForeignPtr s


type CSFML_SoundBuffer = ()
{# pointer *sfSoundBuffer as SoundBufferPtr -> CSFML_SoundBuffer #}
data SoundBuffer = SoundBuffer (ForeignPtr CSFML_SoundBuffer)

withSoundBuffer :: SoundBuffer -> (SoundBufferPtr -> IO a) -> IO a
withSoundBuffer (SoundBuffer b) = withForeignPtr b


type CSFML_SoundBufferRecorder = ()
{# pointer *sfSoundBufferRecorder as SoundBufferRecorderPtr -> CSFML_SoundBufferRecorder #}
data SoundBufferRecorder = SoundBufferRecorder (ForeignPtr CSFML_SoundBufferRecorder)

withSoundBufferRecorder :: SoundBufferRecorder -> (SoundBufferRecorderPtr -> IO a) -> IO a
withSoundBufferRecorder (SoundBufferRecorder b) = withForeignPtr b


type CSFML_SoundRecorder = ()
{# pointer *sfSoundRecorder as SoundRecorderPtr -> CSFML_SoundRecorder #}
data SoundRecorder a = SoundRecorder (ForeignPtr CSFML_SoundRecorder)
                                     (StablePtr (IORef (SoundRecorderCallbackData a)))

data SoundRecorderCallbackData a = SoundRecorderCallbackData a (a -> IO CInt) (Ptr CShort -> CInt -> a -> IO CInt) (a -> IO ())

withSoundRecorder :: SoundRecorder a -> (SoundRecorderPtr -> IO b) -> IO b
withSoundRecorder (SoundRecorder s _) = withForeignPtr s


type CSFML_SoundStream = ()
{# pointer *sfSoundStream as SoundStreamPtr -> CSFML_SoundStream #}
data SoundStream a = SoundStream (ForeignPtr CSFML_SoundStream)
                                 (StablePtr (IORef (SoundStreamCallbackData a)))

data SoundStreamCallbackData a = SoundStreamCallbackData a (SoundStreamChunkPtr -> a -> IO Bool) (TimePtr -> a -> IO ())

newtype SoundStreamChunk = SoundStreamChunk [Int16]
{# pointer *sfSoundStreamChunk as SoundStreamChunkPtr -> SoundStreamChunk #}

instance Storable SoundStreamChunk where
    sizeOf    _ = {# sizeof sfSoundStreamChunk  #}
    alignment _ = {# alignof sfSoundStreamChunk #}
    peek p =
        cIntConv <$> {# get sfSoundStreamChunk->sampleCount #} p >>= \sampleCount ->
        {# get sfSoundStreamChunk->samples #} p >>= \samples ->
        SoundStreamChunk <$> (map cIntConv <$> peekArray sampleCount samples)
    poke p (SoundStreamChunk xs) = do
        {# set sfSoundStreamChunk->sampleCount #} p (cIntConv . length $ xs)
        let arrayPtr = plusPtr p $ sizeOf (1 :: CUInt)
        pokeArray arrayPtr xs

withSoundStream :: SoundStream a -> (SoundStreamPtr -> IO b) -> IO b
withSoundStream (SoundStream s _) = withForeignPtr s

