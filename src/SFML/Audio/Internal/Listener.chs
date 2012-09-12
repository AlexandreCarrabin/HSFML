{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Audio.Internal.Listener where


import Control.Applicative ((<$>))
import Foreign (alloca, peek, with)
import Foreign.C.Types(CFloat)

{# import SFML.System.Internal.Vector3 #} (Vector3D, Vector3FPtr, fromVector3F, toVector3F)

import SFML.Utility.Foreign (cFloatConv)

#include <SFML/Audio/Listener.h>
#include <SFML/Audio/ListenerWrapper.h>


-- |Change the global volume of all the sounds and musics.
-- The volume is a value between 0 and 100, the default value is 100.
setGlobalVolume :: Float -> IO ()
setGlobalVolume volume =
    {# call unsafe sfListener_setGlobalVolume #} (cFloatConv volume)

-- |Get the current value of the global volume.
getGlobalVolume :: IO Float
getGlobalVolume =
    {# call unsafe sfListener_getGlobalVolume #} >>= \res ->
    return $ cFloatConv res

-- |Set the position of the listener in the scene. 
-- The default position is (0, 0, 0).
setPosition :: Vector3D Float -> IO ()
setPosition position =
    with (toVector3F position) $ \p ->
    {# call unsafe sfListener_setPosition_wrapper #} p

-- |Get the current position of the listener in the scene.
getPosition :: IO (Vector3D Float)
getPosition =
    alloca $ \position ->
    {# call unsafe sfListener_getPosition_wrapper #} position >>
    fromVector3F <$> peek position

-- |Set the orientation of the listener in the scene.
-- The orientation defines the 3D axes of the listener in the scene.
-- The orientation vector doesn't have to be normalized.
-- The default listener's orientation is (0, 0, -1).
setDirection :: Vector3D Float -> IO ()
setDirection direction =
    with (toVector3F direction) $ \d ->
    {# call unsafe sfListener_setDirection_wrapper #} d

-- |Get the current orientation of the listener in the scene.
getDirection :: IO (Vector3D Float)
getDirection =
    alloca $ \direction ->
    {# call unsafe sfListener_getDirection_wrapper #} direction >>
    fromVector3F <$> peek direction

