{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Event where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Word (Word)
import Foreign (Ptr, Storable(..))
import Foreign.C.Types (CFloat, CInt, CUInt)

import SFML.System.Vector (Vector2D(..))

{# import SFML.Window.Internal.Joystick #} (JoystickAxis, JoystickButton, Joystick)
{# import SFML.Window.Internal.Keyboard #} (KeyCode, KeyModifiers(..))
{# import SFML.Window.Internal.Mouse #} (MouseButton)

import SFML.Utility.Foreign (cIntConv, cIntToBool, cIntToEnum, cFloatConv)

#include <SFML/Window/Event.h>


{# enum sfEventType as EventType {} with prefix = "sf" #}


data Event = 
             -- |The window requested to be closed.
             Closed
             -- |The window was resized. The parameters are the new width and height of the window.
           | Resized Word Word
             -- |The window lost the focus.
           | LostFocus
             -- |The window gained the focus.
           | GainedFocus
             -- |A character was entered.
           | TextEntered Char
             -- |A key was pressed.
           | KeyPressed KeyCode [KeyModifiers]
             -- |A key was released.
           | KeyReleased KeyCode [KeyModifiers]
             -- |The mouse wheel was moved. The parameters are the number of ticks the wheel has moved (positive is up, negative is down) and the position of the mouse.
           | MouseWheelMoved Int (Vector2D Int)
             -- |A mouse button was pressed.
           | MouseButtonPressed MouseButton (Vector2D Int)
             -- |A mouse button was released.
           | MouseButtonReleased MouseButton (Vector2D Int)
             -- |The mouse cursor moved to given position.
           | MouseMoved (Vector2D Int)
             -- |The mouse cursor entered the area of the window.
           | MouseEntered
             -- |The mouse cursor left the area of the window.
           | MouseLeft
             -- |A joystick was connected.
           | JoystickConnected Joystick
             -- |A joystick was disconnected.
           | JoystickDisconnected Joystick
             -- |A joystick button was pressed.
           | JoystickButtonPressed Joystick JoystickButton
             -- |A joystick button was released.
           | JoystickButtonReleased Joystick JoystickButton
             -- |The joystick moved along an axis.
           | JoystickMoved Joystick JoystickAxis Float

{# pointer *sfEvent as EventPtr -> Event #}


instance Storable Event where
    sizeOf    _ = {# sizeof sfEvent  #}
    alignment _ = {# alignof sfEvent #}
    peek p = do
        evtType <- cIntToEnum `liftM` {# get sfEvent->type #} p
        case evtType of
            EvtClosed                 -> return Closed
            EvtResized                -> peekSize Resized p
            EvtLostFocus              -> return LostFocus
            EvtGainedFocus            -> return GainedFocus
            EvtTextEntered            -> peekText TextEntered p
            EvtKeyPressed             -> peekKey KeyPressed p
            EvtKeyReleased            -> peekKey KeyReleased p
            EvtMouseWheelMoved        -> peekMouseWheel MouseWheelMoved p
            EvtMouseButtonPressed     -> peekMouseButton MouseButtonPressed p
            EvtMouseButtonReleased    -> peekMouseButton MouseButtonReleased p
            EvtMouseMoved             -> peekMouseMove MouseMoved p
            EvtMouseEntered           -> return MouseEntered
            EvtMouseLeft              -> return MouseLeft
            EvtJoystickConnected      -> peekJoystickConnect JoystickConnected p
            EvtJoystickDisconnected   -> peekJoystickConnect JoystickDisconnected p
            EvtJoystickButtonPressed  -> peekJoystickButton JoystickButtonPressed p
            EvtJoystickButtonReleased -> peekJoystickButton JoystickButtonReleased p
            EvtJoystickMoved          -> peekJoystickMove JoystickMoved p
    poke _ _ = undefined

peekSize :: (Word -> Word -> Event) -> EventPtr -> IO Event
peekSize constructor p = constructor
    <$> cIntConv `liftM` {# get sfEvent->size.width  #} p
    <*> cIntConv `liftM` {# get sfEvent->size.height #} p

peekText :: (Char -> Event) -> EventPtr -> IO Event
peekText constructor p = constructor 
    <$> cIntToEnum `liftM` {# get sfEvent->text.unicode #} p

peekKey :: (KeyCode -> [KeyModifiers] -> Event) -> EventPtr -> IO Event
peekKey constructor p = 
    cIntToEnum `liftM` {# get sfEvent->key.code #} p >>= \code ->
    return (constructor code []) >>= \e ->
    cIntToBool `liftM` {# get sfEvent->key.alt #} p >>= \alt ->
    cIntToBool `liftM` {# get sfEvent->key.control #} p >>= \control ->
    cIntToBool `liftM` {# get sfEvent->key.shift   #} p >>= \shift ->
    cIntToBool `liftM` {# get sfEvent->key.system  #} p >>= \system ->
    return . (addModifersIf alt Alt) . (addModifersIf control Control) . (addModifersIf shift Shift) . (addModifersIf system System) $ e
    where
        addModifersIf True m (KeyPressed  c xs) = KeyPressed  c (m:xs)
        addModifersIf True m (KeyReleased c xs) = KeyReleased c (m:xs)
        addModifersIf _ _ event = event

peekMouseWheel :: (Int -> Vector2D Int -> Event) -> EventPtr -> IO Event
peekMouseWheel constructor p = constructor
    <$> cIntConv `liftM` {# get sfEvent->mouseWheel.delta #} p
    <*> (Vector2D <$> cIntConv `liftM` {# get sfEvent->mouseWheel.x #} p
                  <*> cIntConv `liftM` {# get sfEvent->mouseWheel.y #} p)

peekMouseButton :: (MouseButton -> Vector2D Int -> Event) -> EventPtr -> IO Event
peekMouseButton constructor p = constructor
    <$> cIntToEnum `liftM` {# get sfEvent->mouseButton.button #} p
    <*> (Vector2D <$> cIntConv `liftM` {# get sfEvent->mouseButton.x #} p
                  <*> cIntConv `liftM` {# get sfEvent->mouseButton.y #} p)

peekMouseMove :: (Vector2D Int -> Event) -> EventPtr -> IO Event
peekMouseMove constructor p = constructor <$> 
    (Vector2D <$> cIntConv `liftM` {# get sfEvent->mouseMove.x #} p
              <*> cIntConv `liftM` {# get sfEvent->mouseMove.y #} p)

peekJoystickConnect :: (Joystick -> Event) -> EventPtr -> IO Event
peekJoystickConnect constructor p = constructor
    <$> cIntToEnum `liftM` {# get sfEvent->joystickConnect.joystickId #} p

peekJoystickButton :: (Joystick -> JoystickButton -> Event) -> EventPtr -> IO Event
peekJoystickButton constructor p = constructor
    <$> cIntToEnum `liftM` {# get sfEvent->joystickButton.joystickId #} p
    <*> cIntToEnum `liftM` {# get sfEvent->joystickButton.button     #} p
    
peekJoystickMove :: (Joystick -> JoystickAxis -> Float -> Event) -> EventPtr -> IO Event
peekJoystickMove constructor p = constructor
    <$> cIntToEnum `liftM` {# get sfEvent->joystickMove.joystickId #} p
    <*> cIntToEnum `liftM` {# get sfEvent->joystickMove.axis       #} p
    <*> cFloatConv `liftM` {# get sfEvent->joystickMove.position   #} p

