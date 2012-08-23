module SFML.Graphics.TransformableLike
    ( TransformableLike(..)
    ) where


import qualified SFML.Graphics.Circle as Ci
import qualified SFML.Graphics.Convex as Co
import qualified SFML.Graphics.Rectangle as R
import qualified SFML.Graphics.Shape as Sh
import qualified SFML.Graphics.Sprite as Sp
import qualified SFML.Graphics.Text as Te
import qualified SFML.Graphics.Transformable as Tr

import SFML.Graphics.Internal.Transform (Transform)
import SFML.Graphics.Internal.Types (Circle, Convex, Rectangle, Shape, Sprite, Text, Transformable)
import SFML.System.Vector (Vector2D)


class TransformableLike a where
    getOrigin :: a -> IO (Vector2D Float)
    setOrigin :: a -> Vector2D Float -> IO ()
    getPosition :: a -> IO (Vector2D Float)
    setPosition :: a -> Vector2D Float -> IO ()
    move :: a -> Vector2D Float -> IO ()

    getRotation :: a -> IO Float
    setRotation :: a -> Float -> IO ()
    rotate :: a -> Float -> IO ()

    getScale :: a -> IO (Vector2D Float)
    setScale :: a -> Vector2D Float -> IO ()
    scale :: a -> Vector2D Float -> IO ()

    getTransform :: a -> IO Transform
    getInverseTransform :: a -> IO Transform

instance TransformableLike Sprite where
    getOrigin = Sp.getOrigin
    setOrigin = Sp.setOrigin
    getPosition = Sp.getPosition
    setPosition = Sp.setPosition
    move = Sp.move
    getRotation = Sp.getRotation
    setRotation = Sp.setRotation
    rotate = Sp.rotate
    getScale = Sp.getScale
    setScale = Sp.setScale
    scale = Sp.scale
    getTransform = Sp.getTransform
    getInverseTransform = Sp.getInverseTransform

instance TransformableLike Text where
    getOrigin = Te.getOrigin
    setOrigin = Te.setOrigin
    getPosition = Te.getPosition
    setPosition = Te.setPosition
    move = Te.move
    getRotation = Te.getRotation
    setRotation = Te.setRotation
    rotate = Te.rotate
    getScale = Te.getScale
    setScale = Te.setScale
    scale = Te.scale
    getTransform = Te.getTransform
    getInverseTransform = Te.getInverseTransform

instance TransformableLike (Shape a) where
    getOrigin = Sh.getOrigin
    setOrigin = Sh.setOrigin
    getPosition = Sh.getPosition
    setPosition = Sh.setPosition
    move = Sh.move
    getRotation = Sh.getRotation
    setRotation = Sh.setRotation
    rotate = Sh.rotate
    getScale = Sh.getScale
    setScale = Sh.setScale
    scale = Sh.scale
    getTransform = Sh.getTransform
    getInverseTransform = Sh.getInverseTransform

instance TransformableLike Circle where
    getOrigin = Ci.getOrigin
    setOrigin = Ci.setOrigin
    getPosition = Ci.getPosition
    setPosition = Ci.setPosition
    move = Ci.move
    getRotation = Ci.getRotation
    setRotation = Ci.setRotation
    rotate = Ci.rotate
    getScale = Ci.getScale
    setScale = Ci.setScale
    scale = Ci.scale
    getTransform = Ci.getTransform
    getInverseTransform = Ci.getInverseTransform

instance TransformableLike Convex where
    getOrigin = Co.getOrigin
    setOrigin = Co.setOrigin
    getPosition = Co.getPosition
    setPosition = Co.setPosition
    move = Co.move
    getRotation = Co.getRotation
    setRotation = Co.setRotation
    rotate = Co.rotate
    getScale = Co.getScale
    setScale = Co.setScale
    scale = Co.scale
    getTransform = Co.getTransform
    getInverseTransform = Co.getInverseTransform

instance TransformableLike Rectangle where
    getOrigin = R.getOrigin
    setOrigin = R.setOrigin
    getPosition = R.getPosition
    setPosition = R.setPosition
    move = R.move
    getRotation = R.getRotation
    setRotation = R.setRotation
    rotate = R.rotate
    getScale = R.getScale
    setScale = R.setScale
    scale = R.scale
    getTransform = R.getTransform
    getInverseTransform = R.getInverseTransform

instance TransformableLike Transformable where
    getOrigin = Tr.getOrigin
    setOrigin = Tr.setOrigin
    getPosition = Tr.getPosition
    setPosition = Tr.setPosition
    move = Tr.move
    getRotation = Tr.getRotation
    setRotation = Tr.setRotation
    rotate = Tr.rotate
    getScale = Tr.getScale
    setScale = Tr.setScale
    scale = Tr.scale
    getTransform = Tr.getTransform
    getInverseTransform = Tr.getInverseTransform

