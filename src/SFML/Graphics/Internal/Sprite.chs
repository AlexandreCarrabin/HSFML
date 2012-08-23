{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Sprite where 


import Control.Applicative ((<$>), (<*>))
import Data.IORef (IORef, newIORef, writeIORef)
import Foreign (FinalizerPtr, alloca, newForeignPtr, nullPtr, peek, with)
import Foreign.C.Types (CFloat, CInt)

{# import SFML.Graphics.Internal.Rect #} (Rect, fromFloatRect, fromIntRect, toIntRect, FloatRectPtr, IntRectPtr)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_Sprite, Sprite(..), SpritePtr, withSprite, Texture(..), TexturePtr, withTexture)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv, cIntFromBool, ptrToMaybe)

#include <SFML/Graphics/Sprite.h>
#include <SFML/Graphics/SpriteWrapper.h>


getIORefTexture :: Sprite -> IORef (Maybe Texture)
getIORefTexture (Sprite _ t) = t


-- |Create a new `Sprite'.
createSprite :: IO Sprite
createSprite =
    {# call unsafe sfSprite_create #} >>= \res ->
    mkSprite res Nothing

mkSprite :: SpritePtr -> Maybe Texture -> IO Sprite
mkSprite ptr texture = 
    Sprite <$> newForeignPtr c_destroySprite ptr <*> newIORef texture

-- |Create a new `Sprite' by copying an existing one.
copySprite :: Sprite -> IO Sprite
copySprite sprite =
    withSprite sprite $ \s ->
    {# call unsafe sfSprite_copy #} s >>= \copy ->
    getTexture sprite >>= \texture ->
    mkSprite copy texture

-- |Destroy a `Sprite'.
foreign import ccall unsafe "SFML/Graphics/Sprite.h &sfSprite_destroy"
    c_destroySprite :: FinalizerPtr CSFML_Sprite

-- |Set the position of a `Sprite'.
setPosition :: Sprite -> Vector2D Float -> IO ()
setPosition sprite position =
    withSprite sprite $ \s ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfSprite_setPosition_wrapper #} s p

-- |Set the orientation of a `Sprite'.
setRotation :: Sprite -> Float -> IO ()
setRotation sprite angle =
    withSprite sprite $ \s ->
    {# call unsafe sfSprite_setRotation #} s (cFloatConv angle)

-- |Set the scale factors of a `Sprite'.
setScale :: Sprite -> Vector2D Float -> IO ()
setScale sprite scales =
    withSprite sprite $ \sp ->
    with (toVector2F scales) $ \sc ->
    {# call unsafe sfSprite_setScale_wrapper #} sp sc

-- |Set the local origin of a `Sprite'.
setOrigin :: Sprite -> Vector2D Float -> IO ()
setOrigin sprite origin =
    withSprite sprite $ \s ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfSprite_setOrigin_wrapper #} s o

-- |Get the position of a `Sprite'.
getPosition :: Sprite -> IO (Vector2D Float)
getPosition sprite =
    withSprite sprite $ \s ->
    alloca $ \position ->
    {# call unsafe sfSprite_getPosition_wrapper #} s position >>
    fromVector2F <$> peek position

-- |Get the orientation of a `Sprite'.
getRotation :: Sprite -> IO Float
getRotation sprite =
    withSprite sprite $ \s ->
    {# call unsafe sfSprite_getRotation #} s >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of a `Sprite'.
getScale :: Sprite -> IO (Vector2D Float)
getScale sprite =
    withSprite sprite $ \s ->
    alloca $ \scales ->
    {# call unsafe sfSprite_getScale_wrapper #} s scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of a `Sprite'.
getOrigin :: Sprite -> IO (Vector2D Float)
getOrigin sprite =
    withSprite sprite $ \s ->
    alloca $ \origin ->
    {# call unsafe sfSprite_getOrigin_wrapper #} s origin >>
    fromVector2F <$> peek origin

-- |Move a `Sprite' by a given offset.
move :: Sprite -> Vector2D Float -> IO ()
move sprite offset =
    withSprite sprite $ \s ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfSprite_move_wrapper #} s o

-- |Rotate a `Sprite'.
rotate :: Sprite -> Float -> IO ()
rotate sprite angle =
    withSprite sprite $ \s ->
    {# call unsafe sfSprite_rotate #} s (cFloatConv angle)

-- |Scale a `Sprite'.
scale :: Sprite -> Vector2D Float -> IO ()
scale sprite factors =
    withSprite sprite $ \s ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfSprite_scale_wrapper #} s f

-- |Get the combined transform of a `Sprite'.
getTransform :: Sprite -> IO Transform
getTransform sprite =
    withSprite sprite $ \s ->
    alloca $ \transform ->
    {# call unsafe sfSprite_getTransform_wrapper #} s transform >>
    peek transform

-- |Get the inverse of the combined transform of a `Sprite'.
getInverseTransform :: Sprite -> IO Transform
getInverseTransform sprite =
    withSprite sprite $ \s ->
    alloca $ \transform ->
    {# call unsafe sfSprite_getInverseTransform_wrapper #} s transform >>
    peek transform

-- |Change the source `Texture' of a `Sprite'.
setTexture :: Sprite -> Maybe Texture -> Bool -> IO ()
setTexture sprite texture reset = 
    withSprite sprite $ \s ->
    writeIORef (getIORefTexture sprite) texture >>
    case texture of
        Nothing -> {# call unsafe sfSprite_setTexture #} s nullPtr (cIntFromBool reset)
        Just t  -> withTexture t $ \t' -> 
                   {# call unsafe sfSprite_setTexture #} s t' (cIntFromBool reset)

-- |Set the sub-rectangle of the `Texture' that a `Sprite' will display.
setTextureRect :: Sprite -> Rect Int -> IO ()
setTextureRect sprite rect =
    withSprite sprite $ \s ->
    with (toIntRect rect) $ \r ->
    {# call unsafe sfSprite_setTextureRect_wrapper #} s r

-- |Set the global `Color' of a `Sprite'.
setColor :: Sprite -> Color -> IO ()
setColor sprite color =
    withSprite sprite $ \s ->
    with color $ \c ->
    {# call unsafe sfSprite_setColor_wrapper #} s c

--FIXME: Get value from IORef ?
-- |Get the source `Texture' of a `Sprite'.
getTexture :: Sprite -> IO (Maybe Texture)
getTexture sprite =
    withSprite sprite $ \s ->
    {# call unsafe sfSprite_getTexture #} s >>= \res ->
    ptrToMaybe mkConstTexture res

-- |Get the sub-rectangle of the `Texture' displayed by a `Sprite'.
getTextureRect :: Sprite -> IO (Rect Int)
getTextureRect sprite =
    withSprite sprite $ \s ->
    alloca $ \rect ->
    {# call unsafe sfSprite_getTextureRect_wrapper #} s rect >>
    fromIntRect <$> peek rect

-- |Get the global `Color' of a `Sprite'.
getColor :: Sprite -> IO Color
getColor sprite =
    withSprite sprite $ \s ->
    alloca $ \color ->
    {# call unsafe sfSprite_getColor_wrapper #} s color >>
    peek color

-- |Get the local bounding rectangle of the `Sprite'.
getLocalBounds :: Sprite -> IO (Rect Float)
getLocalBounds sprite =
    withSprite sprite $ \s ->
    alloca $ \bounds ->
    {# call unsafe sfSprite_getLocalBounds_wrapper #} s bounds >>
    fromFloatRect <$> peek bounds

-- |Get the global bounding rectangle of the `Sprite'.
getGlobalBounds :: Sprite -> IO (Rect Float)
getGlobalBounds sprite =
    withSprite sprite $ \s ->
    alloca $ \bounds ->
    {# call unsafe sfSprite_getGlobalBounds_wrapper #} s bounds >>
    fromFloatRect <$> peek bounds
