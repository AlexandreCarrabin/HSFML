{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Image where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString, pack)
import Data.Word (Word, Word8)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, peek, peekArray, with)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt, CUInt, CUChar, CULong)

{# import SFML.Graphics.Internal.Rect #} (Rect, toIntRect, IntRectPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_Image, Image(..), ImagePtr, withImage)

{# import SFML.System.Internal.InputStream #} (InputStream, InputStreamPtr, withInputStream)
{# import SFML.System.Internal.Vector2 #} (Vector2D(..), Vector2UPtr, fromVector2U)

import SFML.Utility.Foreign (cIntConv, cIntFromBool, cIntToBool, withByteString, withVoidPtr)

#include <SFML/Graphics/Image.h>
#include <SFML/Graphics/ImageWrapper.h>


-- |Create a new `Image' of the given width and height. It is filled with black pixels.
createImage :: Vector2D Word-> IO Image
createImage (Vector2D width height) =
    {# call unsafe sfImage_create #} (cIntConv width) (cIntConv height) >>= \res ->
    mkImage res

-- |Create a new `Image' of the given width and height and fill it with a unique color.
createImageFromColor :: Vector2D Word -> Color -> IO Image
createImageFromColor (Vector2D width height) color =
    with color $ \c ->
    {# call unsafe sfImage_createFromColor_wrapper #} (cIntConv width) (cIntConv height) c >>= \res ->
    mkImage res

-- |Create a new `Image' of the given width and height from a `ByteString' representing an array of pixels.
--The ByteString must contain 4 elements from each pixels (red, green, blue and alpha) times the number of pixels in the image.
createImageFromPixels :: Vector2D Word -> ByteString -> IO Image
createImageFromPixels (Vector2D width height) pixels =
    withByteString pixels $ \p ->
    {# call unsafe sfImage_createFromPixels #} (cIntConv width) (cIntConv height) p >>= \res ->
    mkImage res

-- |Create a new `Image' from a file. The supported image formats are bmp, png, tga, jpg, gif, psd, hdr and pic.
createImageFromFile :: FilePath -> IO Image
createImageFromFile path =
    withCString path $ \p ->
    {# call unsafe sfImage_createFromFile #} p >>= \res ->
    mkImage res

-- |Create a new `Image' from a file in memory. The supported image formats are bmp, png, tga, jpg, gif, psd, hdr and pic.
createImageFromMemory :: ByteString -> IO Image
createImageFromMemory memory =
    withVoidPtr memory $ \(m, size) ->
    {# call unsafe sfImage_createFromMemory #} m size >>= \res ->
    mkImage res

-- |Create a new `Image' from a custom `InputStream'.
createImageFromStream :: InputStream a -> IO Image
createImageFromStream stream =
    withInputStream stream $ \s ->
    {# call sfImage_createFromStream #} s >>= \res ->
    mkImage res

mkImage :: ImagePtr -> IO Image
mkImage ptr = Image <$> newForeignPtr c_destroyImage ptr

-- |Create a new `Image' by copying an existing one.
copyImage :: Image -> IO Image
copyImage image =
    withImage image $ \i ->
    {# call unsafe sfImage_copy #} i >>= \res ->
    mkImage res

-- |Destroy an `Image'.
foreign import ccall unsafe "SFML/Graphics/Image.h &sfImage_destroy"
    c_destroyImage :: FinalizerPtr CSFML_Image

-- |Save the `Image' to a file on disk.
--The format of the image is automatically deduced from the extension. The supported image formats are bmp, png, tga and jpg.
saveToFile :: Image -> FilePath -> IO Bool
saveToFile image path =
    withImage image $ \i ->
    withCString path $ \p ->
    {# call unsafe sfImage_saveToFile #} i p >>= \res ->
    return $ cIntToBool res

-- |Return the size of the `Image'.
getSize :: Image -> IO (Vector2D Word)
getSize image =
    withImage image $ \i ->
    alloca $ \size ->
    {# call unsafe sfImage_getSize_wrapper #} i size >>
    fromVector2U <$> peek size

-- |Create a transparency mask from a specified color-key.
--This function sets the alpha value of every pixel matching the given `Color' to the given alpha value.
createMaskFromColor :: Image -> Color -> Word8 -> IO ()
createMaskFromColor image color alpha =
    withImage image $ \i ->
    with color $ \c ->
    {# call unsafe sfImage_createMaskFromColor_wrapper #} i c (cIntConv alpha)

-- |Copy pixels from the seconf `Image' onto the first one.
--
--This function does a slow pixel copy and should not be used intensively. It can be used to prepare a complex
--static image from several others, but if you need this kind of feature in real-time you'd better use RenderTexture.
copyToImage :: Image          -- ^Destination image.
            -> Image          -- ^Source image.
            -> Vector2D Word  -- ^X and Y coordinates of the destination position.
            -> Rect Int       -- ^Sub-rectangle of the source image to copy.
            -> Bool           -- ^Should the copy take in account the source transparency?
            -> IO ()
copyToImage dest src (Vector2D destX destY) srcRect applyAlpha =
    withImage dest $ \d ->
    withImage src $ \s ->
    with (toIntRect srcRect) $ \r ->
    {# call unsafe sfImage_copyImage_wrapper #} d s (cIntConv destX) (cIntConv destY) r (cIntFromBool applyAlpha)

-- |Change the `Color' of a pixel in an `Image'.
setPixel :: Image -> Vector2D Word -> Color -> IO ()
setPixel image (Vector2D x y) color =
    withImage image $ \i ->
    with color $ \c ->
    {# call unsafe sfImage_setPixel_wrapper #} i (cIntConv x) (cIntConv y) c

-- |Get the `Color' of a pixel in an `Image'.
getPixel :: Image -> Vector2D Word -> IO Color
getPixel image (Vector2D x y) =
    withImage image $ \i ->
    alloca $ \color ->
    {# call unsafe sfImage_getPixel_wrapper #} i (cIntConv x) (cIntConv y) color >>
    peek color

-- |Get a `ByteString' of the pixels of an `Image'.
--The size of the array is 4 * width * height of the image.
getPixels :: Image -> IO ByteString
getPixels image = 
    getSize image >>= \(Vector2D w h) ->
    withImage image $ \i ->
    {# call unsafe sfImage_getPixelsPtr #} i >>= \ptr ->
    peekArray (fromIntegral (4 * w * h)) ptr >>= \pixels ->
    return ((pack . map cIntConv) pixels)

-- |Flip an `Image' horizontally (left <-> right).
flipHorizontally :: Image -> IO ()
flipHorizontally image =
    withImage image $ \i ->
    {# call unsafe sfImage_flipHorizontally #} i

-- |Flip an `Image' vertically (top <-> bottom).
flipVertically :: Image -> IO ()
flipVertically image =
    withImage image $ \i ->
    {# call unsafe sfImage_flipVertically #} i

