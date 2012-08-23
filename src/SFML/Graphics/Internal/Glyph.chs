{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Glyph where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Foreign (Ptr, Storable(..), alloca, with)
import Foreign.C.Types (CInt)

{# import SFML.Graphics.Internal.Rect #} (IntRectPtr, Rect, fromIntRect, toIntRect)

import SFML.Utility.Foreign (cIntConv)

#include <SFML/Graphics/Glyph.h>
#include <SFML/Graphics/GlyphWrapper.h>


data Glyph = Glyph 
    { getAdvance     :: Int       -- ^Offset to move horizontically to the next character.
    , getBounds      :: Rect Int  -- ^Bounding rectangle of the `Glyph', in coordinates relative to the baseline.
    , getTextureRect :: Rect Int  -- ^Texture coordinates of the `Glyph' inside the font's image.
    }
{# pointer *sfGlyph as GlyphPtr -> Glyph #}

instance Storable Glyph where
    sizeOf    _ = {# sizeof sfGlyph  #}
    alignment _ = {# alignof sfGlyph #}
    peek p = Glyph
        <$> cIntConv `liftM` {# get sfGlyph->advance #} p
        <*> c_getBounds p
        <*> c_getTextureRect p
    poke p (Glyph a b t) = do
        {# set sfGlyph.advance #} p (cIntConv a)
        c_setBounds p b
        c_setTextureRect p t

c_getBounds :: GlyphPtr -> IO (Rect Int)
c_getBounds glyph =
    alloca $ \bounds ->
    {# call unsafe sfGlyph_getBounds_wrapper #} glyph bounds >>
    fromIntRect <$> peek bounds

c_setBounds :: GlyphPtr -> Rect Int -> IO ()
c_setBounds glyph bounds =
    with (toIntRect bounds) $ \b ->
    {# call unsafe sfGlyph_getBounds_wrapper #} glyph b

c_getTextureRect :: GlyphPtr -> IO (Rect Int)
c_getTextureRect glyph =
    alloca $ \rect ->
    {# call unsafe sfGlyph_getTextureRect_wrapper #} glyph rect >>
    fromIntRect <$> peek rect

c_setTextureRect :: GlyphPtr -> Rect Int -> IO ()
c_setTextureRect glyph rect =
    with (toIntRect rect) $ \r ->
    {# call unsafe sfGlyph_setTextureRect_wrapper #} glyph r

