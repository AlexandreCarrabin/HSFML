module SFML.Graphics.Rectangle
    ( Rectangle()
    , createRectangle
    , copyRectangle

    -- TransformableLike
    , setPosition
    , setRotation
    , setScale
    , setOrigin
    , getPosition
    , getRotation
    , getScale
    , getOrigin
    , move
    , rotate
    , scale
    , getTransform
    , getInverseTransform

    -- ShapeLike
    , setTexture
    , setTextureRect
    , setFillColor
    , setOutlineColor
    , setOutlineThickness
    , getTexture
    , getTextureRect
    , getFillColor
    , getOutlineColor
    , getOutlineThickness
    , getPointCount
    , getPoint
    , getLocalBounds
    , getGlobalBounds

    , setSize
    , getSize
    ) where


import SFML.Graphics.Internal.Rectangle
import SFML.Graphics.Internal.Types (Rectangle)

