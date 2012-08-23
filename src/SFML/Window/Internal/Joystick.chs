{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Joystick where


import Foreign.C.Types (CFloat, CInt, CUInt)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromEnum, cIntToBool)

#include <SFML/Window/Joystick.h>


-- |Maximum number of connected joystick (currently 8).
joystickCount :: Int
joystickCount = 8

-- |Maximum number of supported buttons (currently 32).
joystickButtonCount :: Int
joystickButtonCount = 32

-- |Maximum number of supported axes (currently 8).
joystickAxisCount :: Int
joystickAxisCount = 8


newtype Joystick = Joystick Int 
    deriving (Eq)

instance Bounded Joystick where
    minBound = Joystick 0
    maxBound = Joystick (joystickCount - 1)

instance Enum Joystick where
    toEnum x | x >= minBound && x <= maxBound = Joystick x
    toEnum x = error $ "JoystickId : bad argument " ++ show x

    fromEnum (Joystick x) = x


newtype JoystickButton = JoystickButton Int

instance Bounded JoystickButton where
    minBound = JoystickButton 0
    maxBound = JoystickButton (joystickButtonCount - 1)

instance Enum JoystickButton where
    toEnum x | x >= minBound && x <= maxBound = JoystickButton x
    toEnum x = error $ "JoystickButton : bad argument " ++ show x

    fromEnum (JoystickButton x) = x


{# enum sfJoystickAxis as JoystickAxis {} with prefix = "sf" deriving (Show, Eq) #}


-- |Check if a `Joystick' is connected.
isConnected :: Joystick -> IO Bool
isConnected jid =
    {# call unsafe sfJoystick_isConnected #} (cIntFromEnum jid) >>= \res ->
    return $ cIntToBool res

-- |Return the number of buttons supported by a `Joystick'. If the joystick is not connected, the function returns 0.
getButtonCount :: Integral a => Joystick -> IO a
getButtonCount jid =
    {# call unsafe sfJoystick_getButtonCount #} (cIntFromEnum jid) >>= \res ->
    return $ cIntConv res

-- |Check if a `Joystick' supports a given axis. If the joystick is not connected, this function returns `False'.
hasAxis :: Joystick -> JoystickAxis -> IO Bool
hasAxis jid jaxis =
    {# call unsafe sfJoystick_hasAxis #} (cIntFromEnum jid) (cIntFromEnum jaxis) >>= \res ->
    return $ cIntToBool res

-- |Check if a `JoystickButton' is pressed. If the joystick is not connected, this function returns `Falser.
isButtonPressed :: Joystick -> JoystickButton -> IO Bool
isButtonPressed jid jbutton =
    {# call unsafe sfJoystick_isButtonPressed #} (cIntFromEnum jid) (cIntFromEnum jbutton) >>= \res ->
    return $ cIntToBool res

-- |Get the current position of a `JoystickAxis'. If the joystick is not connected, this function returns 0.
getAxisPosition :: Fractional a => Joystick -> JoystickAxis -> IO a
getAxisPosition jid jaxis =
    {# call unsafe sfJoystick_getAxisPosition #} (cIntFromEnum jid) (cIntFromEnum jaxis) >>= \res ->
    return $ cFloatConv res

-- |Update the states of all joysticks.
--
--This function is used internally by SFML, so you normally don't have to call it explicitely. 
--However, you may need to call it if you have no window yet (or no window at all): in this case the joysticks states are not updated automatically.
updateJoystick :: IO ()
updateJoystick = {# call unsafe sfJoystick_update #}

