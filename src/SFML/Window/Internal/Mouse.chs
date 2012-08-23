{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Mouse where


import Control.Applicative ((<$>))
import Foreign (alloca, peek, nullPtr, with)
import Foreign.C.Types (CInt)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2IPtr, fromVector2I, toVector2I)

{# import SFML.Window.Internal.Types #} (Window, WindowPtr, withWindow)

import SFML.Utility.Foreign (cIntFromEnum, cIntToBool)

#include <SFML/Window/Mouse.h>
#include <SFML/Window/MouseWrapper.h>


{# enum sfMouseButton as MouseButton 
    { sfMouseLeft as MouseLeftButton
    , sfMouseRight as MouseRightButton
    , sfMouseMiddle as MouseMiddleButton
    --The other values are left unchanged (with prefix removed)
    } with prefix = "sf" deriving (Show, Eq) #}


-- |Check if a `MouseButton` is pressed. 
isButtonPressed :: MouseButton -> IO Bool
isButtonPressed mb =
    {# call unsafe sfMouse_isButtonPressed #} (cIntFromEnum mb) >>= \res ->
    return $ cIntToBool res

-- |Get the current position of the mouse in desktop coordinates.
getAbsolutePosition :: Integral a => IO (Vector2D a)
getAbsolutePosition =
    alloca $ \vector ->
    {# call unsafe sfMouse_getPosition_wrapper #} nullPtr vector >>
    fromVector2I <$> peek vector

--FIXME: Add a function for RenderWindow ?
-- |Get the current position of the mouse in window coordinates.
getRelativePosition :: Integral a => Window -> IO (Vector2D a)
getRelativePosition window =
    withWindow window $ \w ->
    alloca $ \vector ->
    {# call unsafe sfMouse_getPosition_wrapper #} w vector >>
    fromVector2I <$> peek vector

-- |Set the current position of the mouse in desktop coordinates.
setAbsolutePosition :: Integral a => (Vector2D a) -> IO ()
setAbsolutePosition vector =
    with (toVector2I vector) $ \v ->
    {# call unsafe sfMouse_setPosition_wrapper #} nullPtr v

--FIXME: Add a function for RenderWindow ?
-- |Set the current position of the mouse in window coordinates.
setRelativePosition :: Integral a => Window -> (Vector2D a) -> IO ()
setRelativePosition window vector =
    withWindow window $ \w ->
    with (toVector2I vector) $ \v ->
    {# call unsafe sfMouse_setPosition_wrapper #} w v

