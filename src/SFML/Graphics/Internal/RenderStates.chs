{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.RenderStates where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Foreign (Ptr, Storable(..), alloca, with)
import Foreign.C.Types (CInt)

{# import SFML.Graphics.Internal.BlendMode #} (BlendMode)
{# import SFML.Graphics.Internal.Shader #} (mkShader)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Types #} (Shader, ShaderPtr, withShader, Texture, TexturePtr, withTexture)

import SFML.Utility.Foreign (cIntFromEnum, cIntToEnum, maybeToPtr, ptrToMaybe)

#include <SFML/Graphics/RenderStates.h>
#include <SFML/Graphics/RenderStatesWrapper.h>

-- |Define the states used for drawing to a `RenderTarget'.
data RenderStates = RenderStates
    { getBlendMode :: BlendMode
    , getTransform :: Transform
    , getTexture   :: Maybe Texture
    , getShader    :: Maybe Shader
    }
{# pointer *sfRenderStates as RenderStatesPtr -> RenderStates #}

instance Storable RenderStates where
    sizeOf    _ = {# sizeof sfRenderStates  #}
    alignment _ = {# alignof sfRenderStates #}
    peek p = RenderStates
        <$> cIntToEnum `liftM` {# get sfRenderStates->blendMode #} p
        <*> c_getTransform p
        <*> (ptrToMaybe mkConstTexture =<< {# get sfRenderStates->texture #} p)
        <*> (ptrToMaybe mkShader  =<< {# get sfRenderStates->shader  #} p)
    poke p (RenderStates blendMode transform texture shader) = do
        {# set sfRenderStates.blendMode #} p (cIntFromEnum blendMode)
        c_setTransform p transform
        maybeToPtr texture withTexture (\t -> {# set sfRenderStates.texture #} p t)
        maybeToPtr shader  withShader  (\s -> {# set sfRenderStates.shader  #} p s)

c_getTransform :: RenderStatesPtr -> IO Transform
c_getTransform states =
    alloca $ \transform ->
    {# call unsafe sfRenderStates_getTransform_wrapper #} states transform >>
    peek transform

c_setTransform :: RenderStatesPtr -> Transform -> IO ()
c_setTransform states transform =
    with transform $ \t ->
    {# call unsafe sfRenderStates_setTransform_wrapper #} states t

