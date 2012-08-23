{-# LANGUAGE ForeignFunctionInterface #-}
module SFML.Graphics.Convex 
    ( Convex()
    , createConvex
    , copyConvex

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
    , setPoint
    ) where


import SFML.Graphics.Internal.Convex
import SFML.Graphics.Internal.Types (Convex)

