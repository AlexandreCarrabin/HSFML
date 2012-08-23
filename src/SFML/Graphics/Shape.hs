module SFML.Graphics.Shape 
    ( Shape()
    , CallbackGetPointCount
    , CallbackGetPoint
    , createShape

    -- TransformableLike
    , getOrigin
    , setOrigin
    , getPosition
    , setPosition
    , move
    , getRotation
    , setRotation
    , rotate
    , getScale
    , setScale
    , scale
    , getTransform
    , getInverseTransform

    -- ShapeLike
    , getPoint
    , getPointCount
    , getFillColor
    , setFillColor
    , getOutlineThickness
    , setOutlineThickness
    , getOutlineColor
    , setOutlineColor
    , getTexture
    , setTexture
    , getTextureRect
    , setTextureRect
    , getLocalBounds
    , getGlobalBounds
    ) where


import SFML.Graphics.Internal.Shape
import SFML.Graphics.Internal.Types (Shape)

