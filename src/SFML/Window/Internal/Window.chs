{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Window where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.ByteString (ByteString)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, Storable(..), alloca, newForeignPtr, with)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CUChar, CFloat, CInt, CUInt, CULong)

{# import SFML.System.Internal.Vector2 #} (Vector2D(..), Vector2IPtr, Vector2UPtr, fromVector2I, toVector2I, fromVector2U, toVector2U)

{# import SFML.Window.Internal.Event #} (Event, EventPtr)
{# import SFML.Window.Internal.Types #} (CSFML_Window, Window(..), WindowPtr, withWindow)
{# import SFML.Window.Internal.VideoMode #} (VideoMode, VideoModePtr)
{# import SFML.Window.Internal.WindowHandle #} (WindowHandle)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromEnumList, cIntFromBool, cIntToBool, withByteString)

#include <SFML/Window/Window.h>
#include <SFML/Window/WindowWrapper.h>


data Style = 
             NoStyle  -- ^This style can not be used with another style or it is ignored.
           | Titlebar
           | Resize
           | Close
           | Fullscreen
           deriving (Show, Eq, Bounded, Enum)

-- |Return the default window style : Titlebar + Resize + Close
defaultStyle :: [Style]
defaultStyle = [Titlebar, Resize, Close]


-- |Structure defining the settings of the OpenGL context attached to a window.
--
--`ContextSettings' allows to define several advanced settings of the OpenGL context attached to a window.
--
--All these settings have no impact on the regular SFML rendering (graphics module),
--except the anti-aliasing level, so you may need to use this structure only if you're using SFML as a windowing system for custom OpenGL rendering.
--
--The depthBits and stencilBits members define the number of bits per pixel requested for the (respectively) depth and stencil buffers.
--
--antialiasingLevel represents the requested number of multisampling levels for anti-aliasing.
--
--majorVersion and minorVersion define the version of the OpenGL context that you want. 
--Only versions greater or equal to 3.0 are relevant; versions lesser than 3.0 are all handled the same way
--(i.e. you can use any version < 3.0 if you don't want an OpenGL 3 context).
--
-- /Please note/ that these values are only a hint. 
--No failure will be reported if one or more of these values are not supported by the system.
--Instead, SFML will try to find the closest valid match. 
--You can then retrieve the settings that the window actually used to create its context, with `getSettings'.
data ContextSettings = ContextSettings 
    { depthBits         :: Word
    , stencilBits       :: Word
    , antialiasingLevel :: Word
    , majorVersion      :: Word
    , minorVersion      :: Word
    }
{# pointer *sfContextSettings as ContextSettingsPtr -> ContextSettings #}

instance Storable ContextSettings where
    sizeOf    _ = {# sizeof sfContextSettings  #}
    alignment _ = {# alignof sfContextSettings #}
    peek p = ContextSettings
        <$> cIntConv `liftM` {# get sfContextSettings->depthBits         #} p
        <*> cIntConv `liftM` {# get sfContextSettings->stencilBits       #} p
        <*> cIntConv `liftM` {# get sfContextSettings->antialiasingLevel #} p
        <*> cIntConv `liftM` {# get sfContextSettings->majorVersion      #} p
        <*> cIntConv `liftM` {# get sfContextSettings->minorVersion      #} p
    poke p (ContextSettings d s aa majV minV) = do
        {# set sfContextSettings.depthBits         #} p (cIntConv d)
        {# set sfContextSettings.stencilBits       #} p (cIntConv s)
        {# set sfContextSettings.antialiasingLevel #} p (cIntConv aa)
        {# set sfContextSettings.majorVersion      #} p (cIntConv majV)
        {# set sfContextSettings.minorVersion      #} p (cIntConv minV)


-- |Create (or recreate) the `Window'. If the window was already created, it closes it first.
--If the `Style' list contains `Fullscreen', then the `VideoMode' must be a valid.
createWindow :: VideoMode -> String -> [Style] -> ContextSettings -> IO Window
createWindow mode title styles settings =
    with mode $ \m ->
    withCString title $ \t ->
    with settings $ \s ->
    {# call unsafe sfWindow_create_wrapper #} m t (cIntFromEnumList styles) s >>= \res ->
    mkWindow res

-- |Create (or recreate) the `Window' from an existing control. 
--Use this function if you want to create an OpenGL rendering area into an already existing control.
--If the window was already created, it closes it first.
createWindowFromHandle :: WindowHandle -> ContextSettings -> IO Window
createWindowFromHandle handle settings =
    with settings $ \s ->
    {# call unsafe sfWindow_createFromHandle #} (cIntConv handle) s >>= \res ->
    mkWindow res

mkWindow :: WindowPtr -> IO Window
mkWindow ptr = Window <$> newForeignPtr c_destroyWindow ptr

-- |Destroy a 'Window'.
foreign import ccall unsafe "SFML/Window/Window.h &sfWindow_destroy"
    c_destroyWindow :: FinalizerPtr CSFML_Window

-- |Close the `Window' and destroy all the attached resources.
--
--After calling this function, the `Window' remains valid and you can call `create' to recreate the window.
--All other functions such as `pollEvent' or `display' will still work (i.e. you don't have to test `isOpen' every time), 
--and will have no effect on closed windows.
close :: Window -> IO ()
close window =
    withWindow window $ \w ->
    {# call unsafe sfWindow_close #} w

-- |Tell whether or not the `Window' is open.
--
--This function returns whether or not the window exists. 
--Note that a hidden window (`setVisible' window False) is open (therefore this function would return `True').
isOpen :: Window -> IO Bool
isOpen window =
    withWindow window $ \w ->
    {# call unsafe sfWindow_isOpen #} w >>= \res ->
    return $ cIntToBool res

-- |Get the settings of the OpenGL context of the window.
getSettings :: Window -> IO ContextSettings
getSettings window =
    withWindow window $ \w ->
    alloca $ \settings ->
    {# call unsafe sfWindow_getSettings_wrapper #} w settings >>
    peek settings

-- |Pop the `Event' on top of events stack, if any, and return it. This function is not blocking:
--if there's no pending event then it will return `Nothing'.
pollEvent :: Window -> IO (Maybe Event)
pollEvent w = 
    withWindow w $ \w' ->
    alloca $ \e -> 
    {# call unsafe sfWindow_pollEvent #} w' e >>= \res ->
    if cIntToBool res 
        then Just <$> peek e
        else return Nothing

-- |Wait for an `Event' and return it. This function is blocking:
--if there's no pending event then it will wait until an event is received unless an error occured, in this cas `Nothing' is returned.
waitEvent :: Window -> IO (Maybe Event)
waitEvent w = 
    withWindow w $ \w' ->
    alloca $ \e -> 
    {# call unsafe sfWindow_waitEvent #} w' e >>= \res ->
    if cIntToBool res 
        then Just <$> peek e
        else return Nothing

-- |Get the position of the window.
getPosition :: Window -> IO (Vector2D Int)
getPosition window =
    withWindow window $ \w ->
    alloca $ \position ->
    {# call unsafe sfWindow_getPosition_wrapper #} w position >>
    fromVector2I <$> peek position

-- |Change the position of the `Window' on screen. (Only works for top-level windows)
setPosition :: Window -> Vector2D Int -> IO ()
setPosition window position =
    withWindow window $ \w ->
    with (toVector2I position) $ \p ->
    {# call unsafe sfWindow_setPosition_wrapper #} w p

-- |Get the size of the rendering region of the `Window'.
--The size doesn't include the titlebar and borders of the window.
getSize :: Window -> IO (Vector2D Word)
getSize window =
    withWindow window $ \w ->
    alloca $ \size ->
    {# call unsafe sfWindow_getSize_wrapper #} w size >>
    fromVector2U <$> peek size

-- |Change the size of the rendering region of the `Window'.
setSize :: Window -> Vector2D Word -> IO ()
setSize window size =
    withWindow window $ \w ->
    with (toVector2U size) $ \s ->
    {# call unsafe sfWindow_setSize_wrapper #} w s

-- |Change the title of the `Window'.
setTitle :: Window -> String -> IO ()
setTitle window title =
    withWindow window $ \w ->
    withCString title $ \t ->
    {# call unsafe sfWindow_setTitle #} w t

-- |Change the `Window''s icon. The parameter are the window, the width and height of the icon
--and the pixels array of size width * height * 4 (RGBA format)
setIcon :: Window 
           -> Vector2D Word   -- ^The width and height of the icon
           -> ByteString  -- ^Pixel data
           -> IO ()
setIcon window (Vector2D width height) pixels =
    withWindow window $ \w ->
    withByteString pixels $ \p ->
    {# call unsafe sfWindow_setIcon #} w (cIntConv width) (cIntConv height) p

-- |Show or hide the `Window'. (The window is visible by default)
setVisible :: Window -> Bool -> IO ()
setVisible window visible =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setVisible #} w (cIntFromBool visible)

-- |Show or hide the mouse cursor. (The cursor is visible by default)
setMouseCursorVisible :: Window -> Bool -> IO ()
setMouseCursorVisible window visible =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setMouseCursorVisible #} w (cIntFromBool visible)

-- |Enable or disable vertical synchronization.
--Activating vertical synchronization will limit the number of frames displayed to the refresh rate of the monitor.
--This can avoid some visual artifacts, and limit the framerate to a good value (but not constant across different computers).
--Vertical synchronization is disabled by default.
setVerticalSyncEnabled :: Window -> Bool -> IO ()
setVerticalSyncEnabled window enabled =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setVerticalSyncEnabled #} w (cIntFromBool enabled)

-- |Enable or disable automatic key-repeat.
--If key repeat is enabled, you will receive repeated KeyPressed events while keeping a key pressed. 
--If it is disabled, you will only get a single event when the key is pressed.
--Key repeat is enabled by default.
setKeyRepeatEnabled :: Window -> Bool -> IO ()
setKeyRepeatEnabled window enabled =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setKeyRepeatEnabled #} w (cIntFromBool enabled)

-- |Activate or deactivate the `Window' as the current target for OpenGL rendering.
--A window is active only on the current thread, if you want to make it active on another thread
--you have to deactivate it on the previous thread first if it was active.
--Only one window can be active on a thread at a time, thus the window previously active (if any) automatically gets deactivated.
setActive :: Window -> Bool -> IO Bool
setActive window active =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setActive #} w (cIntFromBool active) >>= \res ->
    return $ cIntToBool res

-- |Display on screen what has been rendered to the `Window' so far.
display :: Window -> IO ()
display window = 
    withWindow window $ \w -> 
    {# call unsafe sfWindow_display #} w

-- |Limit the framerate to a maximum fixed frequency.
setFramerateLimit :: Window -> Word -> IO ()
setFramerateLimit window limit =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setFramerateLimit #} w (cIntConv limit)

-- |Change the joystick threshold.
--The joystick threshold is the value below which no JoystickMoved event will be generated.
--The threshold value is 0.1 by default.
setJoystickThreshold :: Window -> Float -> IO ()
setJoystickThreshold window threshold =
    withWindow window $ \w ->
    {# call unsafe sfWindow_setJoystickThreshold #} w (cFloatConv threshold)

-- |Get the OS-specific handle of the `Window'.
--You shouldn't need to use this function, unless you have very specific stuff to implement that SFML doesn't support,
--or implement a temporary workaround until a bug is fixed.
getSystemHandle :: Window -> IO WindowHandle
getSystemHandle window =
    withWindow window $ \w ->
    {# call unsafe sfWindow_getSystemHandle #} w >>= \res ->
    return $ cIntConv res
 
