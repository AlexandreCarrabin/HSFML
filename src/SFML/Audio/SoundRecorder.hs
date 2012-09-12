module SFML.Audio.SoundRecorder
    ( SoundRecorder
    , SoundRecorderStartCallback
    , SoundRecorderProcessCallback
    , SoundRecorderStopCallback

    , createSoundRecorder

    , start
    , stop

    , getSampleRate
    , isAvailable
    ) where


import SFML.Audio.Internal.SoundRecorder
import SFML.Audio.Internal.Types (SoundRecorder)

