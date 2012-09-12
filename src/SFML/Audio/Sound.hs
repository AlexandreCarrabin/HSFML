module SFML.Audio.Sound
    ( Sound

    , createSound
    , copySound

    , play
    , pause
    , stop

    , getBuffer
    , setBuffer
    , getLoop
    , setLoop
    , getStatus
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
    ) where


import SFML.Audio.Internal.Sound
import SFML.Audio.Internal.Types (Sound)

