module SFML.Window.WindowLike
    ( WindowLike(..)
    ) where


import Data.ByteString (ByteString)
import Data.Word (Word)

import SFML.System.Vector (Vector2D)
import SFML.Window.Event (Event)
import SFML.Window.VideoMode (VideoMode)
import SFML.Window.Window (ContextSettings, Style)
import SFML.Window.WindowHandle (WindowHandle)

import SFML.Graphics.Internal.Types (RenderWindow)
import qualified SFML.Graphics.RenderWindow as RW
import SFML.Window.Internal.Types (Window)
import qualified SFML.Window.Window as W


class WindowLike a where
    createWindow :: VideoMode -> String -> [Style] -> ContextSettings -> IO a
    createWindowFromHandle :: WindowHandle -> ContextSettings -> IO a
    getSystemHandle :: a -> IO WindowHandle
    getSettings :: a -> IO ContextSettings

    close :: a -> IO ()
    isOpen :: a -> IO Bool
    setVisible :: a -> Bool -> IO ()

    pollEvent :: a -> IO (Maybe Event)
    waitEvent :: a -> IO (Maybe Event)

    setTitle :: a -> String -> IO ()
    setIcon :: a -> Vector2D Word -> ByteString -> IO ()

    getPosition :: a -> IO (Vector2D Int)
    setPosition :: a -> Vector2D Int -> IO ()
    getSize :: a -> IO (Vector2D Word)
    setSize :: a -> Vector2D Word -> IO ()

    setJoystickThreshold :: a -> Float -> IO ()
    setKeyRepeatEnabled :: a -> Bool -> IO ()
    setMouseCursorVisible :: a -> Bool -> IO ()
    setFramerateLimit :: a -> Word -> IO ()
    setVerticalSyncEnabled :: a -> Bool -> IO ()
    setActive :: a -> Bool -> IO Bool

    display :: a -> IO ()


instance WindowLike Window where
    createWindow = W.createWindow
    createWindowFromHandle = W.createWindowFromHandle
    getSystemHandle = W.getSystemHandle
    getSettings = W.getSettings
    close = W.close
    isOpen = W.isOpen
    setVisible = W.setVisible
    pollEvent = W.pollEvent
    waitEvent = W.waitEvent
    setTitle = W.setTitle
    setIcon = W.setIcon
    getPosition = W.getPosition
    setPosition = W.setPosition
    getSize = W.getSize
    setSize = W.setSize
    setJoystickThreshold = W.setJoystickThreshold
    setKeyRepeatEnabled = W.setKeyRepeatEnabled
    setMouseCursorVisible = W.setMouseCursorVisible
    setFramerateLimit = W.setFramerateLimit
    setVerticalSyncEnabled = W.setVerticalSyncEnabled
    setActive = W.setActive
    display = W.display


instance WindowLike RenderWindow where
    createWindow = RW.createRenderWindow
    createWindowFromHandle = RW.createRenderWindowFromHandle
    getSystemHandle = RW.getSystemHandle
    getSettings = RW.getSettings
    close = RW.close
    isOpen = RW.isOpen
    setVisible = RW.setVisible
    pollEvent = RW.pollEvent
    waitEvent = RW.waitEvent
    setTitle = RW.setTitle
    setIcon = RW.setIcon
    getPosition = RW.getPosition
    setPosition = RW.setPosition
    getSize = RW.getSize
    setSize = RW.setSize
    setJoystickThreshold = RW.setJoystickThreshold
    setKeyRepeatEnabled = RW.setKeyRepeatEnabled
    setMouseCursorVisible = RW.setMouseCursorVisible
    setFramerateLimit = RW.setFramerateLimit
    setVerticalSyncEnabled = RW.setVerticalSyncEnabled
    setActive = RW.setActive
    display = RW.display

