module SFML.Graphics.Rect 
    ( Rect(..)
    , RectLike(..)
    , createRect

    , right
    , left
    , top
    , bottom
    , width
    , height

    , emptyRect
    ) where


import SFML.Graphics.Internal.Rect

import SFML.System.Vector (Vector2D(..))


-- |Create a `Rect' from the left and top coordinates and width and size
createRect :: a -> a -> a -> a -> Rect a
createRect l t w h = Rect (Vector2D l t) (Vector2D w h)

-- |Get the left coordinate
left :: Rect a -> a
left = v2x . rectOrigin

-- |Get the right coordinate
right :: Num a => Rect a -> a
right rect = (v2x . rectOrigin) rect + width rect

-- |Get the top coordinate
top :: Rect a -> a
top = v2y . rectOrigin

-- |Get the bottom coordinate
bottom :: Num a => Rect a -> a
bottom rect = (v2x . rectOrigin) rect + height rect

-- |Get the width of the `Rect'
width :: Rect a -> a
width = v2x . rectArea

-- |Get the height of the `Rect'
height :: Rect a -> a
height = v2y . rectArea


-- |Create a default empty `Rect'.
emptyRect :: Num a => Rect a
emptyRect = Rect (Vector2D 0 0) (Vector2D 0 0)

