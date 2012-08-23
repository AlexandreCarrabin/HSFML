{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Texture where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, newForeignPtr_, peek, with)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt, CUChar, CUInt, CULong)

{# import SFML.Graphics.Internal.Image #} (mkImage)
{# import SFML.Graphics.Internal.Rect #} (Rect, toIntRect, IntRectPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_Texture, Texture(..), TexturePtr, withTexture , Image, ImagePtr, withImage , RenderWindow, RenderWindowPtr, withRenderWindow)

{# import SFML.System.Internal.InputStream #} (InputStream, InputStreamPtr, withInputStream)
{# import SFML.System.Internal.Vector2 #} (Vector2D(..), Vector2UPtr, fromVector2U)

{# import SFML.Window.Internal.Types #} (Window, WindowPtr, withWindow)

import SFML.Utility.Foreign (cIntConv, cIntFromBool, cIntToBool, withByteString, withVoidPtr)

#include <SFML/Graphics/Texture.h>
#include <SFML/Graphics/TextureWrapper.h>


-- |Create a new `Texture' of the given width and height.
createTexture :: Vector2D Word -> IO Texture
createTexture (Vector2D width height) =
    {# call unsafe sfTexture_create #} (cIntConv width) (cIntConv height) >>= \res ->
    mkTexture res

--FIXME: Rect Int should be Maybe (Rect Int)
-- |Create a new `Texture' from the given area of a file.
--If you want the entire image then you can use `emptyRect'.
--If the area rectangle crosses the bounds of the image, it is adjusted to fit the image size.
createTextureFromFile :: FilePath -> Rect Int -> IO Texture
createTextureFromFile path area =
    withCString path $ \p ->
    with (toIntRect area) $ \a ->
    {# call unsafe sfTexture_createFromFile #} p a >>= \res ->
    mkTexture res

--FIXME: Rect Int should be Maybe (Rect Int)
-- |Create a new `Texture' from the given area of a file in memory.
createTextureFromMemory :: ByteString -> Rect Int -> IO Texture
createTextureFromMemory mem area =
    withVoidPtr mem $ \(m, size) ->
    with (toIntRect area) $ \a ->
    {# call unsafe sfTexture_createFromMemory #} m size a >>= \res ->
    mkTexture res

--FIXME: Rect Int should be Maybe (Rect Int)
-- |Create a new `Texture' from the given area of a custom `InputStream'.
createTextureFromStream :: InputStream a -> Rect Int -> IO Texture
createTextureFromStream stream area =
    withInputStream stream $ \s ->
    with (toIntRect area) $ \a ->
    {# call sfTexture_createFromStream #} s a >>= \res ->
    mkTexture res

--FIXME: Rect Int should be Maybe (Rect Int)
-- |Create a new `Texture' from the given area of an `Image'.
createTextureFromImage :: Image -> Rect Int -> IO Texture
createTextureFromImage image area =
    withImage image $ \i ->
    with (toIntRect area) $ \a ->
    {# call unsafe sfTexture_createFromImage #} i a >>= \res ->
    mkTexture res

mkTexture :: TexturePtr -> IO Texture
mkTexture ptr = Texture <$> newForeignPtr c_destroyTexture ptr

mkConstTexture :: TexturePtr -> IO Texture
mkConstTexture ptr = Texture <$> newForeignPtr_ ptr

-- |Create a new `Texture' from an existing one.
copyTexture :: Texture -> IO Texture
copyTexture texture =
    withTexture texture $ \t ->
    {# call unsafe sfTexture_copy #} t >>= \res ->
    mkTexture res

-- |Destroy a `Texture'.
foreign import ccall unsafe "SFML/Graphics/Texture.h &sfTexture_destroy"
    c_destroyTexture :: FinalizerPtr CSFML_Texture

-- |Return the size of the `Texture'.
getSize :: Texture -> IO (Vector2D Word)
getSize texture =
    withTexture texture $ \t ->
    alloca $ \size ->
    {# call unsafe sfTexture_getSize_wrapper #} t size >>
    fromVector2U <$> peek size

-- |Copy a `Texture''s pixels to an `Image'.
copyToImage :: Texture -> IO Image
copyToImage texture =
    withTexture texture $ \t ->
    {# call unsafe sfTexture_copyToImage #} t >>= \res ->
    mkImage res

-- |Update a `Texture' from a `ByteString' of pixels.
updateFromPixels :: Texture 
                 -> ByteString     -- ^The pixels to copy to the `Texture'.
                 -> Vector2D Word  -- ^Width and height of the pixel region contained in the `ByteString'.
                 -> Vector2D Word  -- ^X and Y offsets in the `Texture' where to copy the source pixels.
                 -> IO ()
updateFromPixels texture pixels (Vector2D width height) (Vector2D x y) =
    withTexture texture $ \t ->
    withByteString pixels $ \p ->
    {# call unsafe sfTexture_updateFromPixels #} t p (cIntConv width) (cIntConv height) (cIntConv x) (cIntConv y)

-- |Update a `Texture' from an `Image'.
updateFromImage :: Texture 
                -> Image          -- ^`Image' to copy to the `Texture'.
                -> Vector2D Word  -- ^X and Y offsets in the `Texture' where to copy the source `Image'.
                -> IO ()
updateFromImage texture image (Vector2D x y) =
    withTexture texture $ \t ->
    withImage image $ \i ->
    {# call unsafe sfTexture_updateFromImage #} t i (cIntConv x) (cIntConv y)

--FIXME; Use WindowLike ?
-- |Update a `Texture' from the contents of a `Window'.
updateFromWindow :: Texture -> Window -> Vector2D Word -> IO ()
updateFromWindow texture window (Vector2D x y) =
    withTexture texture $ \t ->
    withWindow window $ \w ->
    {# call unsafe sfTexture_updateFromWindow #} t w (cIntConv x) (cIntConv y)

--FIXME; Use WindowLike ?
-- |Update a `Texture' from the contents of a `RenderWindow'.
updateFromRenderWindow :: Texture -> RenderWindow -> Vector2D Word -> IO ()
updateFromRenderWindow texture window (Vector2D x y) =
    withTexture texture $ \t ->
    withRenderWindow window $ \w ->
    {# call unsafe sfTexture_updateFromRenderWindow #} t w (cIntConv x) (cIntConv y)

-- |Activate a `Texture' for rendering.
bind :: Texture -> IO ()
bind texture =
    withTexture texture $ \t ->
    {# call unsafe sfTexture_bind #} t

-- |Enable or disable the smooth filter on a `Texture'.
setSmooth :: Texture -> Bool -> IO ()
setSmooth texture smooth =
    withTexture texture $ \t -> 
    {# call unsafe sfTexture_setSmooth #} t (cIntFromBool smooth)

-- |Tell whether the smooth filter is enabled or not for a `Texture'.
isSmooth :: Texture -> IO Bool
isSmooth texture =
    withTexture texture $ \t ->
    {# call unsafe sfTexture_isSmooth #} t >>= \res ->
    return $ cIntToBool res

-- |Enable or disable repeating for a `Texture'. Repeating is disabled by default.
setRepeated :: Texture -> Bool -> IO ()
setRepeated texture repeated =
    withTexture texture $ \t ->
    {# call unsafe sfTexture_setRepeated #} t (cIntFromBool repeated)

-- |Tell whether a `Texture' is repeated or not.
isRepeated :: Texture -> IO Bool
isRepeated texture =
    withTexture texture $ \t ->
    {# call unsafe sfTexture_isRepeated #} t >>= \res ->
    return $ cIntToBool res

-- |Get the maximum `Texture' size allowed.
getMaximumSize :: IO Word
getMaximumSize =
    {# call unsafe sfTexture_getMaximumSize #} >>= \res ->
    return $ cIntConv res
