{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Types 
    ( Circle(..)
    , CSFML_Circle
    , CirclePtr
    , withCircle
    
    , Convex(..)
    , CSFML_Convex
    , ConvexPtr
    , withConvex

    , Font(..)
    , CSFML_Font
    , FontPtr
    , withFont

    , Image(..)
    , CSFML_Image
    , ImagePtr
    , withImage

    , Rectangle(..)
    , CSFML_Rectangle
    , RectanglePtr
    , withRectangle

    , RenderTexture(..)
    , CSFML_RenderTexture
    , RenderTexturePtr
    , withRenderTexture

    , RenderWindow(..)
    , CSFML_RenderWindow
    , RenderWindowPtr
    , withRenderWindow

    , Shader(..)
    , CSFML_Shader
    , ShaderPtr
    , withShader

    , Shape(..)
    , CSFML_Shape
    , ShapePtr
    , withShape
    , ShapeCallbackData(..)

    , Sprite(..)
    , CSFML_Sprite
    , SpritePtr
    , withSprite

    , Text(..)
    , CSFML_Text
    , TextPtr
    , withText

    , Texture(..)
    , CSFML_Texture
    , TexturePtr
    , withTexture

    , Transformable(..)
    , CSFML_Transformable
    , TransformablePtr
    , withTransformable

    , VertexArray(..)
    , CSFML_VertexArray
    , VertexArrayPtr
    , withVertexArray

    , View(..)
    , CSFML_View
    , ViewPtr
    , withView
    ) where


import Data.IORef (IORef)
import Data.Word (Word)
import Foreign (ForeignPtr, Ptr, StablePtr, withForeignPtr) 

import SFML.System.Vector (Vector2D)

#include <SFML/Graphics/Color.h>
#include <SFML/Graphics/Types.h>


type CSFML_Circle = ()
{# pointer *sfCircleShape as CirclePtr -> CSFML_Circle #}
data Circle = Circle (ForeignPtr CSFML_Circle)
                     (IORef (Maybe Texture))

withCircle :: Circle -> (CirclePtr -> IO a) -> IO a
withCircle (Circle c _) = withForeignPtr c


type CSFML_Convex = ()
{# pointer *sfConvexShape as ConvexPtr -> CSFML_Convex #}
data Convex = Convex (ForeignPtr CSFML_Convex)
                     (IORef (Maybe Texture))

withConvex :: Convex -> (ConvexPtr -> IO a) -> IO a
withConvex (Convex c _) = withForeignPtr c


type CSFML_Font = ()
{# pointer *sfFont as FontPtr -> CSFML_Font #}
data Font = Font (ForeignPtr CSFML_Font)

withFont :: Font -> (FontPtr -> IO a) -> IO a
withFont (Font f) = withForeignPtr f


type CSFML_Image = ()
{# pointer *sfImage as ImagePtr -> CSFML_Image #}
data Image = Image (ForeignPtr CSFML_Image)

withImage :: Image -> (ImagePtr -> IO a) -> IO a
withImage (Image i) = withForeignPtr i


type CSFML_Rectangle = ()
{# pointer *sfRectangleShape as RectanglePtr -> CSFML_Rectangle #}
data Rectangle = Rectangle (ForeignPtr CSFML_Rectangle)
                           (IORef (Maybe Texture))

withRectangle :: Rectangle -> (RectanglePtr -> IO a) -> IO a
withRectangle (Rectangle r _) = withForeignPtr r


type CSFML_RenderTexture = ()
{# pointer *sfRenderTexture as RenderTexturePtr -> CSFML_RenderTexture #}
data RenderTexture = RenderTexture (ForeignPtr CSFML_RenderTexture)

withRenderTexture :: RenderTexture -> (RenderTexturePtr -> IO a) -> IO a
withRenderTexture (RenderTexture r) = withForeignPtr r


type CSFML_RenderWindow = ()
{# pointer *sfRenderWindow as RenderWindowPtr -> CSFML_RenderWindow #}
data RenderWindow = RenderWindow (ForeignPtr CSFML_RenderWindow)

withRenderWindow :: RenderWindow -> (RenderWindowPtr -> IO a) -> IO a
withRenderWindow (RenderWindow r) = withForeignPtr r


type CSFML_Shader = ()
{# pointer *sfShader as ShaderPtr -> CSFML_Shader #}
data Shader = Shader (ForeignPtr CSFML_Shader)

withShader :: Shader -> (ShaderPtr -> IO a) -> IO a
withShader (Shader s) = withForeignPtr s


type CSFML_Shape = ()
{# pointer *sfShape as ShapePtr -> CSFML_Shape #}
data Shape a = Shape (ForeignPtr CSFML_Shape)
                     (IORef (Maybe Texture))
                     (StablePtr (IORef (ShapeCallbackData a)))

data ShapeCallbackData a = ShapeCallbackData a (a -> Word) (a -> Word -> Vector2D Float)

withShape :: Shape a -> (ShapePtr -> IO b) -> IO b
withShape (Shape s _ _) = withForeignPtr s


type CSFML_Sprite = ()
{# pointer *sfSprite as SpritePtr -> CSFML_Sprite #}
data Sprite = Sprite (ForeignPtr CSFML_Sprite)
                     (IORef (Maybe Texture))

withSprite :: Sprite -> (SpritePtr -> IO a) -> IO a
withSprite (Sprite s _) = withForeignPtr s


type CSFML_Text = ()
{# pointer *sfText as TextPtr -> CSFML_Text #}
data Text = Text (ForeignPtr CSFML_Text)
                 (IORef (Maybe Font))

withText :: Text -> (TextPtr -> IO a) -> IO a
withText (Text t _) = withForeignPtr t


type CSFML_Texture = ()
{# pointer *sfTexture as TexturePtr -> CSFML_Texture #}
data Texture = Texture (ForeignPtr CSFML_Texture)

withTexture :: Texture -> (TexturePtr -> IO a) -> IO a
withTexture (Texture t) = withForeignPtr t


type CSFML_Transformable = ()
{# pointer *sfTransformable as TransformablePtr -> CSFML_Transformable #}
data Transformable = Transformable (ForeignPtr CSFML_Transformable)

withTransformable :: Transformable -> (TransformablePtr -> IO a) -> IO a
withTransformable (Transformable t) = withForeignPtr t


type CSFML_VertexArray = ()
{# pointer *sfVertexArray as VertexArrayPtr -> CSFML_VertexArray #}
data VertexArray = VertexArray (ForeignPtr CSFML_VertexArray)

withVertexArray :: VertexArray -> (VertexArrayPtr -> IO a) -> IO a
withVertexArray (VertexArray v) = withForeignPtr v


type CSFML_View = ()
{# pointer *sfView as ViewPtr -> CSFML_View #}
data View = View (ForeignPtr CSFML_View)

withView :: View -> (ViewPtr -> IO a) -> IO a
withView (View v) = withForeignPtr v

