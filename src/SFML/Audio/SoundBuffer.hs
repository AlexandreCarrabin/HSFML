module SFML.Audio.SoundBuffer
    ( SoundBuffer

    , createSoundBufferFromFile
    , createSoundBufferFromMemory
    , createSoundBufferFromStream
    , createSoundBufferFromSamples
    , copySoundBuffer
    , saveToFile

    , getSamples
    , getSampleCount
    , getSampleRate
    , getChannelCount
    , getDuration
    ) where


import SFML.Audio.Internal.SoundBuffer
import SFML.Audio.Internal.Types (SoundBuffer)

