{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.SoundStatus where


#include <SFML/Audio/SoundStatus.h>


{# enum sfSoundStatus as SoundStatus {} with prefix = "sf" deriving (Eq, Show) #}

