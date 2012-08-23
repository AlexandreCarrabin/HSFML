{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Shader where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString, useAsCString)
import Foreign (FinalizerPtr, Ptr, newForeignPtr, with)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CFloat, CInt)

{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_Shader, Shader(..), ShaderPtr, withShader , Texture, TexturePtr, withTexture)

{# import SFML.System.Internal.InputStream #} (InputStream, InputStreamPtr, withInputStream)
{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, toVector2F)
{# import SFML.System.Internal.Vector3 #} (Vector3D, Vector3FPtr, toVector3F)

import SFML.Utility.Foreign (cFloatConv, cIntToBool)

#include <SFML/Graphics/Shader.h>
#include <SFML/Graphics/ShaderWrapper.h>


--FIXME: return IO (Maybe Shader) as the function can fail. Same thing for the arguments.
-- |Load both the vertex and fragment shaders from files. The function returns Nothing if it failed.
--
--This function can load both the vertex and the fragment shaders, or only one of them: pass Nothing if you don't want to load
--either the vertex shader or the fragment shader.  The sources must be text files containing valid shaders GLSL language.
createShaderFromFile :: FilePath -> FilePath -> IO Shader
createShaderFromFile vertex fragment =
    withCString vertex $ \v ->
    withCString fragment $ \f ->
    {# call unsafe sfShader_createFromFile #} v f >>= \res ->
    mkShader res

--FIXME: return IO (Maybe Shader) as the function can fail. Same thing for the arguments.
-- |Load both the vertex and fragment shaders from sources in memory. The function returns Nothing if it failed.
--
--This function can load both the vertex and the fragment shaders, or only one of them: pass Nothing if you don't want to load
--either the vertex shader or the fragment shader.  The sources must be text files containing valid shaders GLSL language.
createShaderFromMemory :: ByteString -> ByteString -> IO Shader
createShaderFromMemory vertex fragment =
    useAsCString vertex $ \v ->
    useAsCString fragment $ \f ->
    {# call unsafe sfShader_createFromMemory #} v f >>= \res ->
    mkShader res

--FIXME: return IO (Maybe Shader) as the function can fail. Same thing for the arguments.
-- |Load both the vertex and fragment shaders from custom `InputStream's. The function returns Nothing if it failed.
--
--This function can load both the vertex and the fragment shaders, or only one of them: pass Nothing if you don't want to load
--either the vertex shader or the fragment shader.  The sources must be text files containing valid shaders GLSL language.
createShaderFromStream :: InputStream a -> InputStream b -> IO Shader
createShaderFromStream vertex fragment =
    withInputStream vertex $ \v ->
    withInputStream fragment $ \f ->
    {# call sfShader_createFromStream #} v f >>= \res ->
    mkShader res

mkShader :: ShaderPtr -> IO Shader
mkShader ptr = Shader <$> newForeignPtr c_destroyShader ptr

-- |Destroy a `Shader'.
foreign import ccall unsafe "SFML/Graphics/Shader.h &sfShader_destroy"
    c_destroyShader :: FinalizerPtr CSFML_Shader

-- |Change the value of the given variable of a `Shader' with the float parameter.
setFloatParameter :: Shader -> String -> Float -> IO ()
setFloatParameter shader name x =
    withShader shader $ \s ->
    withCString name $ \n ->
    {# call unsafe sfShader_setFloatParameter #} s n (cFloatConv x)

-- |Change the value of the given variable of a `Shader' with the 2-components vector parameter.
setFloat2Parameter :: Shader -> String -> Float -> Float -> IO ()
setFloat2Parameter shader name x y =
    withShader shader $ \s ->
    withCString name $ \n ->
    {# call unsafe sfShader_setFloat2Parameter #} s n (cFloatConv x) (cFloatConv y)

-- |Change the value of the given variable of a `Shader' with the 3-components vector parameter.
setFloat3Parameter :: Shader -> String -> Float -> Float -> Float -> IO ()
setFloat3Parameter shader name x y z =
    withShader shader $ \s ->
    withCString name $ \n ->
    {# call unsafe sfShader_setFloat3Parameter #} s n (cFloatConv x) (cFloatConv y) (cFloatConv z)

-- |Change the value of the given variable of a `Shader' with the 4-components vector parameter.
setFloat4Parameter :: Shader -> String -> Float -> Float -> Float -> Float -> IO ()
setFloat4Parameter shader name x y z w =
    withShader shader $ \s ->
    withCString name $ \n ->
    {# call unsafe sfShader_setFloat4Parameter #} s n (cFloatConv x) (cFloatConv y) (cFloatConv z) (cFloatConv w)

-- |Change the value of the given variable of a `Shader' with the 2-components vector parameter.
setVector2Parameter :: Shader -> String -> Vector2D Float -> IO ()
setVector2Parameter shader name vector =
    withShader shader $ \s ->
    withCString name $ \n ->
    with (toVector2F vector) $ \v ->
    {# call unsafe sfShader_setVector2Parameter_wrapper #} s n v

-- |Change the value of the given variable of a `Shader' with the 3-components vector parameter.
setVector3Parameter :: Shader -> String -> Vector3D Float -> IO ()
setVector3Parameter shader name vector =
    withShader shader $ \s ->
    withCString name $ \n ->
    with (toVector3F vector) $ \v ->
    {# call unsafe sfShader_setVector3Parameter_wrapper #} s n v

-- |Change a color parameter of a `Shader'.
setColorParameter :: Shader -> String -> Color -> IO ()
setColorParameter shader name color =
    withShader shader $ \s ->
    withCString name $ \n ->
    with color $ \c ->
    {# call unsafe sfShader_setColorParameter_wrapper #} s n c

-- |Change a matrix parameter of a `Shader'.
setTransformParameter :: Shader -> String -> Transform -> IO ()
setTransformParameter shader name transform =
    withShader shader $ \s ->
    withCString name $ \n ->
    with transform $ \t ->
    {# call unsafe sfShader_setTransformParameter_wrapper #} s n t

-- |Change a `Texture' parameter of a `Shader'.
setTextureParameter :: Shader -> String -> Texture -> IO ()
setTextureParameter shader name texture =
    withShader shader $ \s ->
    withCString name $ \n ->
    withTexture texture $ \t ->
    {# call unsafe sfShader_setTextureParameter #} s n t

-- |Change a `Texture' parameter of a `Shader'.
--
--This function maps a shader texture variable to the texture of the object being drawn, which cannot be known in advance.
setCurrentTextureParameter :: Shader -> String -> IO ()
setCurrentTextureParameter shader name =
    withShader shader $ \s ->
    withCString name $ \n ->
    {# call unsafe sfShader_setCurrentTextureParameter #} s n

-- |Bind a `Shader' for rendering (activate it).
bind :: Shader -> IO ()
bind shader =
    withShader shader $ \s ->
    {# call unsafe sfShader_bind #} s

-- |Unbind a `Shader' (deactivate it).
unbind :: Shader -> IO ()
unbind shader =
    withShader shader $ \s ->
    {# call unsafe sfShader_unbind #} s

-- |Tell whether or not the system supports shaders.
isShaderAvailable :: IO Bool
isShaderAvailable =
    {# call unsafe sfShader_isAvailable #} >>= \res ->
    return $ cIntToBool res

