{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.VertexArray where


import Control.Applicative ((<$>))
import Data.Word (Word)
import Foreign (FinalizerPtr, alloca, newForeignPtr, peek, with)
import Foreign.C.Types (CInt, CUInt)

{# import SFML.Graphics.Internal.PrimitiveType #} (PrimitiveType)
{# import SFML.Graphics.Internal.Rect #} (Rect, fromFloatRect, FloatRectPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_VertexArray, VertexArray(..), VertexArrayPtr, withVertexArray)
{# import SFML.Graphics.Internal.Vertex #} (Vertex, VertexPtr)

import SFML.Utility.Foreign (cIntConv, cIntFromEnum, cIntToEnum)

#include <SFML/Graphics/VertexArray.h>
#include <SFML/Graphics/VertexArrayWrapper.h>


-- |Create a new `VertexArray'.
createVertexArray :: IO VertexArray
createVertexArray =
    {# call unsafe sfVertexArray_create #} >>= \res ->
    mkVertexArray res

mkVertexArray :: VertexArrayPtr -> IO VertexArray
mkVertexArray ptr = VertexArray <$> newForeignPtr c_destroyVertexArray ptr

-- |Create a new `VertexArray' from an existing one.
copyVertexArray :: VertexArray -> IO VertexArray
copyVertexArray array =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_copy #} a >>= \res ->
    mkVertexArray res

-- |Destroy a `VertexArray'.
foreign import ccall unsafe "SFML/Graphics/VertexArray.h &sfVertexArray_destroy"
    c_destroyVertexArray :: FinalizerPtr CSFML_VertexArray

-- |Return the `Vertex' count of a `VertexArray'.
getVertexCount :: VertexArray -> IO Word
getVertexCount array =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_getVertexCount #} a >>= \res ->
    return $ cIntConv res

-- |Get access to a `Vertex' by its index.
getVertex :: VertexArray -> Word -> IO Vertex
getVertex array index =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_getVertex #} a (cIntConv index) >>= \res ->
    peek res

-- |Clear a `VertexArray'.
clear :: VertexArray -> IO ()
clear array =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_clear #} a

-- |Resize the `VertexArray'.
--
--If the new size is greater than the current size, the previous vertices are kept and new (default-constructed) vertices are added.
--If the new size is less than the current size, existing vertices are removed from the array.
resize :: VertexArray -> Word -> IO ()
resize array size =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_resize #} a (cIntConv size)

-- |Add a `Vertex' to a `VertexArray'.
append :: VertexArray -> Vertex -> IO ()
append array vertex =
    withVertexArray array $ \a ->
    with vertex $ \v ->
    {# call unsafe sfVertexArray_append_wrapper #} a v

-- |Set the `PrimitiveType' of a `VertexArray'.
setPrimitiveType :: VertexArray -> PrimitiveType -> IO ()
setPrimitiveType array primitive =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_setPrimitiveType #} a (cIntFromEnum primitive)

-- |Get the `PrimitiveType' drawn by a `VertexArray'.
getPrimitiveType :: VertexArray -> IO PrimitiveType
getPrimitiveType array =
    withVertexArray array $ \a ->
    {# call unsafe sfVertexArray_getPrimitiveType #} a >>= \res ->
    return $ cIntToEnum res

-- |Compute the bounding rectangle of a `VertexArray'.
getBounds :: VertexArray -> IO (Rect Float)
getBounds array =
    withVertexArray array $ \a ->
    alloca $ \bounds ->
    {# call unsafe sfVertexArray_getBounds_wrapper #} a bounds >>
    fromFloatRect <$> peek bounds
