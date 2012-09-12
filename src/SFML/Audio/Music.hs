module SFML.Audio.Music
    ( Music

    , createMusicFromFile
    , createMusicFromMemory
    , createMusicFromStream

    , play
    , pause
    , stop

    , getDuration
    , getChannelCount
    , getSampleRate
    , getStatus

    , getLoop
    , setLoop
    , getPlayingOffset
    , setPlayingOffset
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
    ) where


import SFML.Audio.Internal.Music
import SFML.Audio.Internal.Types (Music)

