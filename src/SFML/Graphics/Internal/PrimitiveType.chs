{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.PrimitiveType where


#include <SFML/Graphics/PrimitiveType.h>


{# enum sfPrimitiveType as PrimitiveType {} with prefix = "sf" deriving (Show, Eq) #}

