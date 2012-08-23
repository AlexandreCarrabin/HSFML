{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Time where


import Control.Applicative ((<$>))
import Foreign (Ptr, Storable(..), alloca, with)
import Foreign.C.Types(CLLong, CFloat, CInt)

import SFML.Utility.Foreign (cFloatConv, cIntConv)

#include <SFML/System/Time.h>
#include <SFML/System/TimeWrapper.h>


newtype Time = Time CLLong
    deriving (Num, Integral, Real, Eq, Ord, Enum, Show)
{# pointer *sfTime as TimePtr -> Time #}

instance Storable Time where
    sizeOf    _ = {# sizeof sfTime  #}
    alignment _ = {# alignof sfTime #}
    peek p = Time <$> {# get sfTime->microseconds #} p
    poke p (Time t) = {# set sfTime.microseconds #} p t

-- |Predefined /zero/ time value.
zero :: Time
zero = Time 0

-- |Return a'Time' value as a number of seconds.
toSeconds :: Fractional a => Time -> IO a
toSeconds time =
    with time $ \ptr ->
    {# call unsafe sfTime_asSeconds_wrapper #} ptr >>= \res ->
    return $ cFloatConv res

-- |Return a'Time' value as a number of milliseconds.
toMilliseconds :: Integral a => Time -> IO a
toMilliseconds time =
    with time $ \ptr ->
    {# call unsafe sfTime_asMilliseconds_wrapper #} ptr >>= \res ->
    return $ cIntConv res

-- |Return a'Time' value as a number of microseconds.
toMicroseconds :: Integral a => Time -> IO a
toMicroseconds time =
    with time $ \ptr ->
    {# call unsafe sfTime_asMicroseconds_wrapper #} ptr >>= \res ->
    return $ cIntConv res

-- |Construct a 'Time' value from a number of seconds.
fromSeconds :: RealFrac a => a -> IO Time
fromSeconds s =
    alloca $ \time -> do
    {# call unsafe sfSeconds_wrapper #} (cFloatConv s) time
    peek time

-- |Construct a 'Time' value from a number of milliseconds.
fromMilliseconds :: Integral a => a -> IO Time
fromMilliseconds ms =
    alloca $ \time -> do
    {# call unsafe sfMilliseconds_wrapper #} (cIntConv ms) time
    peek time

-- |Construct a 'Time' value from a number of microseconds.
fromMicroseconds :: Integral a => a -> IO Time
fromMicroseconds ms =
    alloca $ \time -> do
    {# call unsafe sfMicroseconds_wrapper #} (cIntConv ms) time
    peek time

