module SFML.Graphics.Shader 
    ( Shader()
    , createShaderFromFile
    , createShaderFromMemory
    , createShaderFromStream

    , setFloatParameter
    , setFloat2Parameter
    , setFloat3Parameter
    , setFloat4Parameter

    , setVector2Parameter
    , setVector3Parameter

    , setColorParameter
    , setTransformParameter
    , setTextureParameter
    , setCurrentTextureParameter

    , bind
    , unbind
    , isShaderAvailable
    ) where


import SFML.Graphics.Internal.Shader
import SFML.Graphics.Internal.Types (Shader)

