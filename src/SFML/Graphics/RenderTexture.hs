module SFML.Graphics.RenderTexture 
    ( RenderTexture()
    , createRenderTexture

    , getSize
    , setActive
    , display
    , clear
    , setView
    , getView
    , getDefaultView
    , getViewPort
    , convertCoords
    , drawSprite
    , drawText
    , drawShape
    , drawCircle
    , drawConvex
    , drawRectangle
    , drawVertexArray
    , drawPrimitives
    , pushGLStates
    , popGLStates
    , resetGLStates
    , getTexture
    , setSmooth
    , isSmooth
    ) where


import SFML.Graphics.Internal.RenderTexture
import SFML.Graphics.Internal.Types (RenderTexture)

