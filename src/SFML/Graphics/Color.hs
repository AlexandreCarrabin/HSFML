module SFML.Graphics.Color 
    ( Color(..)

    , sfBlack
    , sfWhite
    , sfRed
    , sfGreen
    , sfBlue
    , sfYellow
    , sfMagenta
    , sfCyan
    , sfTransparent

    , fromRGB
    , fromRGBA
    , add
    , modulate
    ) where


import Data.Word (Word8)

import SFML.Graphics.Internal.Color hiding (fromRGB, fromRGBA, add, modulate)


-- |Construct a `Color' from its 3 RGB components.
fromRGB :: Word8 -> Word8 -> Word8 -> Color
fromRGB r g b = Color r g b 255

-- |Construct a `Color' from its 4 RGBA components.
fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
fromRGBA = Color

-- |Add two `Color's.
add :: Color -> Color -> Color
add (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
    Color (addComponent r1 r2)
          (addComponent g1 g2)
          (addComponent b1 b2)
          (addComponent a1 a2)
    where
        addComponent :: Word8 -> Word8 -> Word8
        addComponent c1 c2 = fromIntegral $
            min (255 :: Int) (fromIntegral c1 + fromIntegral c2)

-- |Modulate two `Color's.
modulate :: Color -> Color -> Color
modulate (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
    Color (modulateComponent r1 r2)
          (modulateComponent g1 g2)
          (modulateComponent b1 b2)
          (modulateComponent a1 a2)
    where 
        modulateComponent :: Word8 -> Word8 -> Word8
        modulateComponent c1 c2 = fromIntegral $ 
            (fromIntegral c1 * fromIntegral c2) `div` (255 :: Int)

