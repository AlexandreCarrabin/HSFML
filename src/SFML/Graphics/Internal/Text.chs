{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Text where


import Control.Applicative ((<$>), (<*>))
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, nullPtr, peek, with)
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CChar, CFloat, CUInt, CULong)

{# import SFML.Graphics.Internal.Rect #} (FloatRectPtr, Rect, fromFloatRect)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Font #} (mkConstFont)
{# import SFML.Graphics.Internal.Types #} (Font, FontPtr, withFont, CSFML_Text, Text(..), TextPtr, withText)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromEnumList, cIntToEnumList, peekUnicodeString, ptrToMaybe, withUnicodeString)

#include <SFML/Graphics/Text.h>
#include <SFML/Graphics/TextWrapper.h>


{# enum sfTextStyle as TextStyle {} with prefix = "sf" deriving (Show, Eq, Bounded) #}


getIORefFont :: Text -> IORef (Maybe Font)
getIORefFont (Text _ f) = f


-- |Create a new `Text'.
createText :: IO Text
createText =
    {# call unsafe sfText_create #} >>= \res ->
    mkText res Nothing

mkText :: TextPtr -> Maybe Font -> IO Text
mkText ptr font = 
    Text <$> newForeignPtr c_destroyText ptr <*> newIORef font

-- |Create a new `Text' from an existing one.
copyText :: Text -> IO Text
copyText text =
    withText text $ \t ->
    {# call unsafe sfText_copy #} t >>= \copy ->
    getFont text >>= \font ->
    mkText copy font

-- |Destroy a `Text'.
foreign import ccall unsafe "SFML/Graphics/Text.h &sfText_destroy"
    c_destroyText :: FinalizerPtr CSFML_Text

-- |Set the position of a `Text'.
setPosition :: Text -> Vector2D Float -> IO ()
setPosition text position =
    withText text $ \t ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfText_setPosition_wrapper #} t p

-- |Set the orientation of a `Text'.
setRotation :: Text -> Float -> IO ()
setRotation text angle =
    withText text $ \t ->
    {# call unsafe sfText_setRotation #} t (cFloatConv angle)

-- |Set the scale factors of a `Text'.
setScale :: Text -> Vector2D Float -> IO ()
setScale text scales =
    withText text $ \t ->
    with (toVector2F scales) $ \s ->
    {# call unsafe sfText_setScale_wrapper #} t s

-- |Set the local origin of a `Text'.
setOrigin :: Text -> Vector2D Float -> IO ()
setOrigin text origin =
    withText text $ \t ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfText_setOrigin_wrapper #} t o

-- |Get the position of a `Text'.
getPosition :: Text -> IO (Vector2D Float)
getPosition text =
    withText text $ \t ->
    alloca $ \position ->
    {# call unsafe sfText_getPosition_wrapper #} t position >>
    fromVector2F <$> peek position

-- |Get the orientation of a `Text'.
getRotation :: Text -> IO Float
getRotation text =
    withText text $ \t ->
    {# call unsafe sfText_getRotation #} t >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of a `Text'.
getScale :: Text -> IO (Vector2D Float)
getScale text =
    withText text $ \t ->
    alloca $ \scales ->
    {# call unsafe sfText_getScale_wrapper #} t scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of a `Text'.
getOrigin :: Text -> IO (Vector2D Float)
getOrigin text =
    withText text $ \t ->
    alloca $ \origin ->
    {# call unsafe sfText_getOrigin_wrapper #} t origin >>
    fromVector2F <$> peek origin

-- |Move a `Text' by a given offset.
move :: Text -> Vector2D Float -> IO ()
move text offset =
    withText text $ \t ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfText_move_wrapper #} t o

-- |Rotate a `Text'.
rotate :: Text -> Float -> IO ()
rotate text angle =
    withText text $ \t ->
    {# call unsafe sfText_rotate #} t (cFloatConv angle)

-- |Scale a `Text'.
scale :: Text -> Vector2D Float -> IO ()
scale text factors =
    withText text $ \t ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfText_scale_wrapper #} t f

-- |Get the combined `Transform' of a `Text'.
getTransform :: Text -> IO Transform
getTransform text =
    withText text $ \t ->
    alloca $ \transform ->
    {# call unsafe sfText_getTransform_wrapper #} t transform >>
    peek transform

-- |Get the inverse of the combined `Transform' of a `Text'.
getInverseTransform :: Text -> IO Transform
getInverseTransform text =
    withText text $ \t ->
    alloca $ \transform ->
    {# call unsafe sfText_getInverseTransform_wrapper #} t transform >>
    peek transform

--FIXME: How to be sure that all char are ASCII ?
-- |Set the string of a `Text' (from an ANSI string).
setString :: Text -> String -> IO ()
setString text string =
    withText text $ \t ->
    withCString string $ \s ->
    {# call unsafe sfText_setString #} t s

-- |Set the string of a `Text' (from a unicode string).
setUnicodeString :: Text -> String -> IO ()
setUnicodeString text string =
    withText text $ \t ->
    withUnicodeString string $ \s ->
    {# call unsafe sfText_setUnicodeString #} t s

-- |Set the `Font' of a `Text'.
setFont :: Text -> Maybe Font -> IO ()
setFont text font =
    withText text $ \t ->
    writeIORef (getIORefFont text) font >>
    case font of
        Nothing -> {# call unsafe sfText_setFont #} t nullPtr
        Just f  -> withFont f $ \f' -> {# call unsafe sfText_setFont #} t f'

-- |Set the character size of a `Text'. The default size is 30.
setCharacterSize :: Text -> Word -> IO ()
setCharacterSize text size =
    withText text $ \t ->
    {# call unsafe sfText_setCharacterSize #} t (cIntConv size)

-- |Set the style of a `Text'. You can pass a combination of one or more `TextStyle's.
-- The default is [`TextRegular'].
setStyle :: Text -> [TextStyle] -> IO ()
setStyle text styles =
    withText text $ \t ->
    {# call unsafe sfText_setStyle #} t (cIntFromEnumList styles)

-- |Set the global `Color' of a `Text'.
setColor :: Text -> Color -> IO ()
setColor text color =
    withText text $ \t ->
    with color $ \c ->
    {# call unsafe sfText_setColor_wrapper #} t c

-- |Get the string of a `Text' (returns an ANSI string).
getString :: Text -> IO String
getString text =
    withText text $ \t ->
    {# call unsafe sfText_getString #} t >>= \res ->
    peekCString res

-- |Get the string of a `Text' (returns a unicode string).
getUnicodeString :: Text -> IO String
getUnicodeString text =
    withText text $ \t ->
    {# call unsafe sfText_getUnicodeString #} t >>= \res ->
    peekUnicodeString res

--FIXME: Get value from IORef ?
-- |Get the `Font' used by a `Text'.
getFont :: Text -> IO (Maybe Font)
getFont text =
    withText text $ \t ->
    {# call unsafe sfText_getFont #} t >>= \res ->
    ptrToMaybe mkConstFont res

-- |Get the size of the characters of a `Text'.
getCharacterSize :: Text -> IO Word
getCharacterSize text =
    withText text $ \t ->
    {# call unsafe sfText_getCharacterSize #} t >>= \res ->
    return $ cIntConv res

-- |Get the style of a `Text'.
getStyle :: Text -> IO [TextStyle]
getStyle text =
    withText text $ \t ->
    {# call unsafe sfText_getStyle #} t >>= \res ->
    return $ cIntToEnumList res

-- |Get the global `Color' of a `Text'.
getColor :: Text -> IO Color
getColor text =
    withText text $ \t ->
    alloca $ \color ->
    {# call unsafe sfText_getColor_wrapper #} t color >>
    peek color

-- |Return the position of the index-th character in a `Text'.
--
--This function computes the visual position of a character from its index in the string.
--The returned position is in global coordinates (translation, rotation, scale and origin are applied).
--If the index is out of range, the position of the end of the string is returned.
findCharacterPos :: Text -> Word -> IO (Vector2D Float)
findCharacterPos text index =
    withText text $ \t ->
    alloca $ \position ->
    {# call unsafe sfText_findCharacterPos_wrapper #} t (cIntConv index) position >>
    fromVector2F <$> peek position

-- |Get the local bounding rectangle of a `Text'.
getLocalBounds :: Text -> IO (Rect Float)
getLocalBounds text =
    withText text $ \t ->
    alloca $ \bounds ->
    {# call unsafe sfText_getLocalBounds_wrapper #} t bounds >>
    fromFloatRect <$> peek bounds

-- |Get the global bounding rectangle of a `Text'.
getGlobalBounds :: Text -> IO (Rect Float)
getGlobalBounds text =
    withText text $ \t ->
    alloca $ \bounds ->
    {# call unsafe sfText_getGlobalBounds_wrapper #} t bounds >>
    fromFloatRect <$> peek bounds

