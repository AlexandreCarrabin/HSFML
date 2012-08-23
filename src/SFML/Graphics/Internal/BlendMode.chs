{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.BlendMode where


#include <SFML/Graphics/BlendMode.h>


-- |Available blending modes for drawing.
{# enum sfBlendMode as BlendMode {} with prefix = "sf" deriving (Show, Eq) #}

