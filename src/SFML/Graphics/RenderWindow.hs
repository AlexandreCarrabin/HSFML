module SFML.Graphics.RenderWindow 
    ( RenderWindow()
    , createRenderWindow
    , createRenderWindowFromHandle

    , close
    , isOpen
    , getSettings
    , pollEvent
    , waitEvent
    , getPosition
    , setPosition
    , getSize
    , setSize
    , setTitle
    , setIcon
    , setVisible
    , setMouseCursorVisible
    , setVerticalSyncEnabled
    , setKeyRepeatEnabled
    , setActive
    , display
    , setFramerateLimit
    , setJoystickThreshold
    , getSystemHandle

    , clear
    , setView
    , getView
    , getDefaultView
    , getViewport
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

    , capture
    ) where


import SFML.Graphics.Internal.RenderWindow
import SFML.Graphics.Internal.Types (RenderWindow)

