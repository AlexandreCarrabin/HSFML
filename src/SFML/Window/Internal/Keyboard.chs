{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Keyboard where


import Foreign.C.Types (CInt)

import SFML.Utility.Foreign (cIntFromEnum, cIntToBool)

#include <SFML/Window/Keyboard.h>


{# enum sfKeyCode as KeyCode {} with prefix = "sf" deriving (Eq, Show) #}

data KeyModifiers = Alt | Control | Shift | System
    deriving (Eq, Enum)

-- |Check if a key is pressed.
isKeyPressed :: KeyCode -> IO Bool
isKeyPressed k =
    {# call unsafe sfKeyboard_isKeyPressed #} (cIntFromEnum k) >>= \res ->
    return $ cIntToBool res

