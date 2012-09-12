module SFML.Audio.SoundStream
    ( SoundStream
    , SoundStreamGetDataCallback
    , SoundStreamSeekCallback

    , createSoundStream

    , play
    , pause
    , stop

    , getStatus
    , getChannelCount
    , getSampleRate

    , getPitch
    , setPitch
    , getVolume
    , setVolume
    , getPosition
    , setPosition
    , isRelativeToListener
    , setRelativeToListener
    , getMinDistance
    , setMinDistance
    , getAttenuation
    , setAttenuation
    , getPlayingOffset
    , setPlayingOffset
    , getLoop
    , setLoop
    ) where


import SFML.Audio.Internal.SoundStream
import SFML.Audio.Internal.Types (SoundStream)

