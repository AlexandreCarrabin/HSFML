module SFML.Window.Window 
    ( Style(..)
    , defaultStyle

    , ContextSettings(..)

    , Window
    , createWindow
    , createWindowFromHandle

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
    ) where


import SFML.Window.Internal.Window
import SFML.Window.Internal.Types (Window)

