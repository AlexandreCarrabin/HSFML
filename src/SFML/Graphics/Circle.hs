module SFML.Graphics.Circle
    ( Circle()
    , createCircle
    , copyCircle

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

    , setPointCount
    , setRadius
    , getRadius
    ) where


import SFML.Graphics.Internal.Circle
import SFML.Graphics.Internal.Types (Circle)

