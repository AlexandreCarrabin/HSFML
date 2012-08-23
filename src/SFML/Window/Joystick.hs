module SFML.Window.Joystick 
    ( Joystick
    , JoystickAxis(..)
    , JoystickButton(..)
    
    , joystickCount
    , joystickAxisCount
    , joystickButtonCount

    , isConnected
    , getButtonCount
    , hasAxis
    , isButtonPressed
    , getAxisPosition

    , updateJoystick
    ) where


import SFML.Window.Internal.Joystick

