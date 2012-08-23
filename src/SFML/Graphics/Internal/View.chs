{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.View where


import Control.Applicative ((<$>))
import Foreign (FinalizerPtr, alloca, newForeignPtr, peek, with)
import Foreign.C.Types (CFloat)

{# import SFML.Graphics.Internal.Rect #} (Rect, fromFloatRect, toFloatRect, FloatRectPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_View, View(..), ViewPtr, withView)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv)


#include <SFML/Graphics/View.h>
#include <SFML/Graphics/ViewWrapper.h>


-- |Create a new `View'.
createView :: IO View
createView =
    {# call unsafe sfView_create #} >>= \res ->
    mkView res

-- |Create a new `View' from a `Rect'.
createViewFromRect :: Rect Float -> IO View
createViewFromRect rect =
    with (toFloatRect rect) $ \r ->
    {# call unsafe sfView_createFromRect_wrapper #} r >>= \res ->
    mkView res

mkView :: ViewPtr -> IO View
mkView ptr = View <$> newForeignPtr c_destroyView ptr

-- |Create a new `View' from an existing one.
copyView :: View -> IO View
copyView view =
    withView view $ \v ->
    {# call unsafe sfView_copy #} v >>= \res ->
    mkView res

-- |Destroy a `View'.
foreign import ccall unsafe "SFML/Graphics/View.h &sfView_destroy"
    c_destroyView :: FinalizerPtr CSFML_View

-- |Set the center of a `View'.
setCenter :: View -> Vector2D Float -> IO ()
setCenter view center =
    withView view $ \v ->
    with (toVector2F center) $ \c ->
    {# call unsafe sfView_setCenter_wrapper #} v c

-- |Set the size of a `View'.
setSize :: View -> Vector2D Float -> IO ()
setSize view size =
    withView view $ \v ->
    with (toVector2F size) $ \s ->
    {# call unsafe sfView_setSize_wrapper #} v s

-- |Set the orientation of a `View'.
setRotation :: View -> Float -> IO ()
setRotation view angle =
    withView view $ \v ->
    {# call unsafe sfView_setRotation #} v (cFloatConv angle)

-- |Set the target viewport of a `View'.
--
--The viewport is the rectangle into which the contents of the view are displayed, 
--expressed as a factor (between 0 and 1) of the size of the render target to which the view is applied.
--For example, a view which takes the left side of the target would be defined by a rect of (0, 0, 0.5, 1).
--By default, a view has a viewport which covers the entire target.
setViewport :: View -> Rect Float -> IO ()
setViewport view viewport =
    withView view $ \v ->
    with (toFloatRect viewport) $ \vp ->
    {# call unsafe sfView_setViewport_wrapper #} v vp

-- |Reset a `View' to the given rectangle. Note that this function resets the rotation angle to 0.
reset :: View -> Rect Float -> IO ()
reset view rect =
    withView view $ \v ->
    with (toFloatRect rect) $ \r ->
    {# call unsafe sfView_reset_wrapper #} v r

-- |Get the center of a `View'.
getCenter :: View -> IO (Vector2D Float)
getCenter view =
    withView view $ \v ->
    alloca $ \center ->
    {# call unsafe sfView_getCenter_wrapper #} v center >>
    fromVector2F <$> peek center

-- |Get the size of a `View'.
getSize :: View -> IO (Vector2D Float)
getSize view =
    withView view $ \v ->
    alloca $ \size ->
    {# call unsafe sfView_getSize_wrapper #} v size >>
    fromVector2F <$> peek size

-- |Get the current orientation of a `View'.
getRotation :: View -> IO Float
getRotation view =
    withView view $ \v ->
    {# call unsafe sfView_getRotation #} v >>= \res ->
    return $ cFloatConv res

-- |Get the target viewport rectangle of a `View'.
getViewport :: View -> IO (Rect Float)
getViewport view =
    withView view $ \v ->
    alloca $ \viewport ->
    {# call unsafe sfView_getViewport_wrapper #} v viewport >>
    fromFloatRect <$> peek viewport

-- |Move a `View' relatively to its current position.
move :: View -> Vector2D Float -> IO ()
move view offset =
    withView view $ \v ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfView_move_wrapper #} v o

-- |Rotate a `View' relatively to its current orientation.
rotate :: View -> Float -> IO ()
rotate view angle =
    withView view $ \v ->
    {# call unsafe sfView_rotate #} v (cFloatConv angle)

-- |Resize a `View' rectangle relatively to its current size.
zoom :: View -> Float -> IO ()
zoom view factor =
    withView view $ \v ->
    {# call unsafe sfView_zoom #} v (cFloatConv factor)
