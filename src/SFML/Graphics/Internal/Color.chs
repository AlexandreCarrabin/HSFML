{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Color where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Word (Word8)
import Foreign (Ptr, Storable(..), alloca, with)
import Foreign.C.Types(CUChar)

import SFML.Utility.Foreign (cIntConv)

#include <SFML/Graphics/Color.h>
#include <SFML/Graphics/ColorWrapper.h>


data Color = Color
    { red   :: Word8
    , green :: Word8
    , blue  :: Word8
    , alpha :: Word8
    } deriving (Eq, Show)
{# pointer *sfColor as ColorPtr -> Color #}

instance Storable Color where
    sizeOf    _ = {# sizeof sfColor  #}
    alignment _ = {# alignof sfColor #}
    peek p = Color
        <$> cIntConv `liftM` {# get sfColor->r #} p
        <*> cIntConv `liftM` {# get sfColor->g #} p
        <*> cIntConv `liftM` {# get sfColor->b #} p
        <*> cIntConv `liftM` {# get sfColor->a #} p
    poke p (Color r g b a) = do
        {# set sfColor.r #} p (cIntConv r)
        {# set sfColor.g #} p (cIntConv g)
        {# set sfColor.b #} p (cIntConv b)
        {# set sfColor.a #} p (cIntConv a)


sfBlack, sfWhite, sfRed, sfGreen, sfBlue, sfYellow, sfMagenta, sfCyan, sfTransparent :: Color
sfBlack       = Color   0   0   0 255
sfWhite       = Color 255 255 255 255
sfRed         = Color 255   0   0 255
sfGreen       = Color   0 255   0 255
sfBlue        = Color   0   0 255 255
sfYellow      = Color 255 255   0 255
sfMagenta     = Color 255   0 255 255
sfCyan        = Color   0 255 255 255
sfTransparent = Color   0   0   0   0


-- |Construct a `Color' from its 3 RGB components.
fromRGB :: Word8 -> Word8 -> Word8 -> IO Color
fromRGB r g b =
    alloca $ \color ->
    {# call unsafe sfColor_fromRGB_wrapper #} (cIntConv r) (cIntConv g) (cIntConv b) color >>
    peek color

-- |Construct a `Color' from its 4 RGBA components.
fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> IO Color
fromRGBA r g b a =
    alloca $ \color ->
    {# call unsafe sfColor_fromRGBA_wrapper #} (cIntConv r) (cIntConv g) (cIntConv b) (cIntConv a) color >>
    peek color

-- |Add two `Color's.
add :: Color -> Color -> IO Color
add color1 color2 =
    with color1 $ \c1 ->
    with color2 $ \c2 ->
    alloca $ \color ->
    {# call unsafe sfColor_add_wrapper #} c1 c2 color >>
    peek color

-- |Modulate two `Color's.
modulate :: Color -> Color -> IO Color
modulate color1 color2 =
    with color1 $ \c1 ->
    with color2 $ \c2 ->
    alloca $ \color ->
    {# call unsafe sfColor_modulate_wrapper #} c1 c2 color >>
    peek color

