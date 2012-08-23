{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Font where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, newForeignPtr_, peek)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt, CUInt, CULong)

{# import SFML.Graphics.Internal.Glyph #} (Glyph, GlyphPtr)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Types #} (CSFML_Font, Font(..), FontPtr, withFont, Texture, TexturePtr)

{# import SFML.System.Internal.InputStream #} (InputStream, InputStreamPtr, withInputStream)

import SFML.Utility.Foreign (cIntConv, cIntFromBool, toCodePoint, withVoidPtr)

#include <SFML/Graphics/Font.h>
#include <SFML/Graphics/FontWrapper.h>


-- |Create a new `Font' from a file.
createFontFromFile :: FilePath -> IO Font
createFontFromFile path =
    withCString path $ \p ->
    {# call unsafe sfFont_createFromFile #} p >>= \res ->
    mkFont res

-- |Create a new `Font' from a file in memory.
createFontFromMemory :: ByteString -> IO Font
createFontFromMemory bytes =
    withVoidPtr bytes $ \(ptr, size) ->
    {# call unsafe sfFont_createFromMemory #} ptr size >>= \res ->
    mkFont res

-- |Create a new `Font' from a custom stream.
createFontFromStream :: InputStream a -> IO Font
createFontFromStream stream =
    withInputStream stream $ \s ->
    {# call sfFont_createFromStream #} s >>= \res ->
    mkFont res

mkFont :: FontPtr -> IO Font
mkFont ptr = Font <$> newForeignPtr c_destroyFont ptr

mkConstFont :: FontPtr -> IO Font
mkConstFont ptr = Font <$> newForeignPtr_ ptr

-- |Create a new `Font' by copying an existing one.
copyFont :: Font -> IO Font
copyFont font =
    withFont font $ \f ->
    {# call unsafe sfFont_copy #} f >>= \res ->
    mkFont res

-- |Destroy a `Font'.
foreign import ccall unsafe "SFML/Graphics/Font.h &sfFont_destroy"
    c_destroyFont :: FinalizerPtr CSFML_Font

-- |Retrieve a `Glyph' of the `Font'.
getGlyph :: Font -> Char -> Word -> Bool -> IO Glyph
getGlyph font char size  bold =
    withFont font $ \f ->
    alloca $ \glyph ->
    {# call unsafe sfFont_getGlyph_wrapper #} f (toCodePoint char) (cIntConv size) (cIntFromBool bold) glyph >>
    peek glyph

-- |Get the kerning offset of two `Glyph's.
getKerning :: Font -> Char -> Char -> Word -> IO Int
getKerning font first second size =
    withFont font $ \f ->
    {# call unsafe sfFont_getKerning #} f (toCodePoint first) (toCodePoint second) (cIntConv size) >>= \res ->
    return $ cIntConv res

-- |Get the spacing between two lines.
getLineSpacing :: Font -> Word -> IO Int
getLineSpacing font size =
    withFont font $ \f ->
    {# call unsafe sfFont_getLineSpacing #} f (cIntConv size) >>= \res ->
    return $ cIntConv res

-- |Retrieve the `Texture' containing the loaded glyphs of a certain size.
getTexture :: Font -> Word -> IO Texture
getTexture font size =
    withFont font $ \f ->
    {# call unsafe sfFont_getTexture #} f (cIntConv size) >>= \res ->
    mkConstTexture res

