module SFML.Graphics.ShapeLike
    ( ShapeLike(..)
    ) where


import Data.Word (Word)

import qualified SFML.Graphics.Circle as Ci
import qualified SFML.Graphics.Convex as Co
import qualified SFML.Graphics.Rectangle as R
import qualified SFML.Graphics.Shape as S

import SFML.Graphics.Color (Color)
import SFML.Graphics.Rect (Rect)
import SFML.Graphics.Internal.Types (Circle, Convex, Rectangle, Shape, Texture)

import SFML.System.Vector (Vector2D)

class ShapeLike a where
    getPoint :: a -> Word -> IO (Vector2D Float)
    getPointCount :: a -> IO Word

    getFillColor :: a -> IO Color
    setFillColor :: a -> Color -> IO ()
    getOutlineThickness :: a -> IO Float
    setOutlineThickness :: a -> Float -> IO ()
    getOutlineColor :: a -> IO Color
    setOutlineColor :: a -> Color -> IO ()

    getTexture :: a -> IO (Maybe Texture)
    setTexture :: a -> Maybe Texture -> Bool -> IO ()
    getTextureRect :: a -> IO (Rect Int)
    setTextureRect :: a -> Rect Int -> IO ()

    getLocalBounds :: a -> IO (Rect Float)
    getGlobalBounds :: a -> IO (Rect Float)

instance ShapeLike (Shape a) where
    getPoint = S.getPoint
    getPointCount = S.getPointCount
    getFillColor = S.getFillColor
    setFillColor = S.setFillColor
    getOutlineThickness = S.getOutlineThickness
    setOutlineThickness = S.setOutlineThickness
    getOutlineColor = S.getOutlineColor
    setOutlineColor = S.setOutlineColor
    getTexture = S.getTexture
    setTexture = S.setTexture
    getTextureRect = S.getTextureRect
    setTextureRect = S.setTextureRect
    getLocalBounds = S.getLocalBounds
    getGlobalBounds = S.getGlobalBounds

instance ShapeLike Circle where
    getPoint = Ci.getPoint
    getPointCount = Ci.getPointCount
    getFillColor = Ci.getFillColor
    setFillColor = Ci.setFillColor
    getOutlineThickness = Ci.getOutlineThickness
    setOutlineThickness = Ci.setOutlineThickness
    getOutlineColor = Ci.getOutlineColor
    setOutlineColor = Ci.setOutlineColor
    getTexture = Ci.getTexture
    setTexture = Ci.setTexture
    getTextureRect = Ci.getTextureRect
    setTextureRect = Ci.setTextureRect
    getLocalBounds = Ci.getLocalBounds
    getGlobalBounds = Ci.getGlobalBounds

instance ShapeLike Convex where
    getPoint = Co.getPoint
    getPointCount = Co.getPointCount
    getFillColor = Co.getFillColor
    setFillColor = Co.setFillColor
    getOutlineThickness = Co.getOutlineThickness
    setOutlineThickness = Co.setOutlineThickness
    getOutlineColor = Co.getOutlineColor
    setOutlineColor = Co.setOutlineColor
    getTexture = Co.getTexture
    setTexture = Co.setTexture
    getTextureRect = Co.getTextureRect
    setTextureRect = Co.setTextureRect
    getLocalBounds = Co.getLocalBounds
    getGlobalBounds = Co.getGlobalBounds

instance ShapeLike Rectangle where
    getPoint = R.getPoint
    getPointCount = R.getPointCount
    getFillColor = R.getFillColor
    setFillColor = R.setFillColor
    getOutlineThickness = R.getOutlineThickness
    setOutlineThickness = R.setOutlineThickness
    getOutlineColor = R.getOutlineColor
    setOutlineColor = R.setOutlineColor
    getTexture = R.getTexture
    setTexture = R.setTexture
    getTextureRect = R.getTextureRect
    setTextureRect = R.setTextureRect
    getLocalBounds = R.getLocalBounds
    getGlobalBounds = R.getGlobalBounds

