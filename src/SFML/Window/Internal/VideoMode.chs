{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.VideoMode where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Word (Word)
import Foreign (Ptr, Storable(..), alloca, peekArray, with)
import Foreign.C.Types (CInt, CUInt, CULong)

import SFML.Utility.Foreign (cIntConv, cIntToBool)

#include <SFML/Window/VideoMode.h>
#include <SFML/Window/VideoModeWrapper.h>


data VideoMode = VideoMode 
    { width        :: Word
    , height       :: Word
    , bitsPerPixel :: Word
    }
{# pointer *sfVideoMode as VideoModePtr -> VideoMode #}

instance Storable VideoMode where
    sizeOf    _ = {# sizeof sfVideoMode  #}
    alignment _ = {# alignof sfVideoMode #}
    peek p = VideoMode
        <$> cIntConv `liftM` {# get sfVideoMode->width        #} p
        <*> cIntConv `liftM` {# get sfVideoMode->height       #} p
        <*> cIntConv `liftM` {# get sfVideoMode->bitsPerPixel #} p
    poke p (VideoMode w h bpp) = do
        {# set sfVideoMode.width        #} p (cIntConv w)
        {# set sfVideoMode.height       #} p (cIntConv h)
        {# set sfVideoMode.bitsPerPixel #} p (cIntConv bpp)

-- |Get the current desktop `VideoMode'.
getDesktopMode :: IO VideoMode
getDesktopMode =
    alloca $ \mode ->
    {# call unsafe sfVideoMode_getDesktopMode_wrapper #} mode >>
    peek mode

-- |Retrieve all the video modes supported in fullscreen mode.
-- |The returned array is sorted from best to worst, so that the first element will always give the best mode (higher width, height and bits-per-pixel).
getFullscreenModes :: IO [VideoMode]
getFullscreenModes =
    alloca $ \psize ->
    {# call unsafe sfVideoMode_getFullscreenModes #} psize >>= \modes ->
    peek psize >>= \size ->
    peekArray (cIntConv size) modes

-- |Tell whether or not the `VideoMode' is valid. The result is only relevant when using fullscreen windows.
isModeValid :: VideoMode -> IO Bool
isModeValid mode =
    with mode $ \m ->
    {# call unsafe sfVideoMode_isValid_wrapper #} m >>= \res ->
    return $ cIntToBool res

