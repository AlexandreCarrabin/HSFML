{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Rect where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Word (Word)
import Foreign (Ptr, Storable(..), alloca, with)
import Foreign.C.Types (CFloat, CInt)

import SFML.System.Vector (Vector2D(..))

import SFML.Utility.Foreign (cIntConv, cIntToBool, cFloatConv)

#include <SFML/Graphics/Rect.h>


data Rect a = Rect 
    { rectOrigin :: Vector2D a
    , rectArea   :: Vector2D a
    } deriving (Eq, Show)

fromIntRect :: Integral a => IntRect -> Rect a
fromIntRect (IntRect l t w h) = 
    Rect (Vector2D (cIntConv l) (cIntConv t))
         (Vector2D (cIntConv w) (cIntConv h))

toIntRect :: Integral a => Rect a -> IntRect
toIntRect (Rect (Vector2D l t) (Vector2D w h)) =
    IntRect (cIntConv l) (cIntConv t) (cIntConv w) (cIntConv h)

fromFloatRect :: RealFrac a => FloatRect -> Rect a
fromFloatRect (FloatRect l t w h) = 
    Rect (Vector2D (cFloatConv l) (cFloatConv t))
         (Vector2D (cFloatConv w) (cFloatConv h))

toFloatRect :: RealFrac a => Rect a -> FloatRect
toFloatRect (Rect (Vector2D l t) (Vector2D w h)) =
    FloatRect (cFloatConv l) (cFloatConv t) (cFloatConv w) (cFloatConv h)


class RectLike a where
    -- |Check if a point is inside the area of a `Rect'.
    contains   :: Rect a -> Vector2D a -> Bool
    -- |Get the intersection between two `Rect's.
    intersects :: Rect a -> Rect a -> Maybe (Rect a)

instance RectLike Int where
    contains   = rectContains
    intersects = rectIntersects
        
instance RectLike Integer where
    contains   = rectContains
    intersects = rectIntersects
        
instance RectLike Word where
    contains   = rectContains
    intersects = rectIntersects
        
instance RectLike Float where
    contains   = rectContains
    intersects = rectIntersects
        
instance RectLike Double where
    contains   = rectContains
    intersects = rectIntersects


data IntRect = IntRect CInt CInt CInt CInt
{# pointer *sfIntRect as IntRectPtr -> IntRect #}

instance Storable IntRect where
    sizeOf    _ = {# sizeof sfIntRect  #}
    alignment _ = {# alignof sfIntRect #}
    peek p = IntRect 
        <$> {# get sfIntRect->left   #} p
        <*> {# get sfIntRect->top    #} p
        <*> {# get sfIntRect->width  #} p
        <*> {# get sfIntRect->height #} p
    poke p (IntRect l t w h) = do
        {# set sfIntRect.left   #} p l
        {# set sfIntRect.top    #} p t
        {# set sfIntRect.width  #} p w
        {# set sfIntRect.height #} p h

-- sfBool sfIntRect_contains(const sfIntRect* rect, int x, int y);
intRectContains :: Integral a => Rect a -> Vector2D a -> IO Bool
intRectContains rect (Vector2D x y) =
    with (toIntRect rect) $ \r ->
    {# call unsafe sfIntRect_contains #} r (cIntConv x) (cIntConv y) >>= \res ->
    return $ cIntToBool res

-- sfBool sfIntRect_intersects(const sfIntRect* rect1, const sfIntRect* rect2, sfIntRect* intersection);
intRectIntersects :: Integral a => Rect a -> Rect a -> IO (Maybe (Rect a))
intRectIntersects rect1 rect2 =
    with (toIntRect rect1) $ \r1 ->
    with (toIntRect rect2) $ \r2 ->
    alloca $ \intersection ->
    {# call unsafe sfIntRect_intersects #} r1 r2 intersection >>= \res ->
    case (cIntToBool res) of
        True  -> peek intersection >>= return . Just . fromIntRect
        False -> return Nothing


data FloatRect = FloatRect Float Float Float Float
{# pointer *sfFloatRect as FloatRectPtr -> FloatRect #}

instance Storable FloatRect where
    sizeOf    _ = {# sizeof sfFloatRect  #}
    alignment _ = {# alignof sfFloatRect #}
    peek p = FloatRect
        <$> cFloatConv `liftM` {# get sfFloatRect->left   #} p
        <*> cFloatConv `liftM` {# get sfFloatRect->top    #} p
        <*> cFloatConv `liftM` {# get sfFloatRect->width  #} p
        <*> cFloatConv `liftM` {# get sfFloatRect->height #} p
    poke p (FloatRect l t w h) = do
        {# set sfFloatRect.left   #} p (cFloatConv l)
        {# set sfFloatRect.top    #} p (cFloatConv t)
        {# set sfFloatRect.width  #} p (cFloatConv w)
        {# set sfFloatRect.height #} p (cFloatConv h)

-- sfBool sfFloatRect_contains(const sfFloatRect* rect, float x, float y);
floatRectContains :: RealFrac a => Rect a -> Vector2D a -> IO Bool
floatRectContains rect (Vector2D x y) =
    with (toFloatRect rect) $ \r ->
    {# call unsafe sfFloatRect_contains #} r (cFloatConv x) (cFloatConv y) >>= \res ->
    return $ cIntToBool res

-- sfBool sfFloatRect_intersects(const sfFloatRect* rect1, const sfFloatRect* rect2, sfFloatRect* intersection);
floatRectIntersects :: RealFrac a => Rect a -> Rect a -> IO (Maybe (Rect a))
floatRectIntersects rect1 rect2 =
    with (toFloatRect rect1) $ \r1 ->
    with (toFloatRect rect2) $ \r2 ->
    alloca $ \intersection ->
    {# call unsafe sfFloatRect_intersects #} r1 r2 intersection >>= \res ->
    case (cIntToBool res) of
        True  -> peek intersection >>= return . Just . fromFloatRect
        False -> return Nothing


rectContains :: (Num a, Ord a) => Rect a -> Vector2D a -> Bool
rectContains (Rect (Vector2D l t) (Vector2D w h)) (Vector2D x y) =
    (x >= l) && (x < l + w) && ( y >= t) && (y < t + h)

rectIntersects :: (Num a, Ord a) => Rect a -> Rect a -> Maybe (Rect a)
rectIntersects (Rect (Vector2D l1 t1) (Vector2D w1 h1))
               (Rect (Vector2D l2 t2) (Vector2D w2 h2)) =
    let interLeft   = max l1 l2
        interTop    = max t1 t2
        interRight  = min (l1 + w1) (l2 + w2)
        interBottom = min (t1 + h1) (t2 + h2)
    in case (interLeft < interRight) && (interTop < interBottom) of
        True  -> Just $ Rect (Vector2D interLeft interTop) (Vector2D (interRight - interLeft) (interBottom - interTop))
        False -> Nothing

