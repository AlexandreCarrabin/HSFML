module SFML.System.Time 
    ( Time
    , zero

    , fromSeconds
    , fromMilliseconds
    , fromMicroseconds

    , toSeconds
    , toMilliseconds
    , toMicroseconds
    ) where


import SFML.System.Internal.Time (Time(..), zero)


-- |Return a'Time' value as a number of seconds.
toSeconds :: Fractional a => Time -> a
toSeconds (Time t) = fromIntegral t / 1000000

-- |Return a'Time' value as a number of milliseconds.
toMilliseconds :: Integral a => Time -> a
toMilliseconds (Time t) = fromIntegral $ t `quot` 1000

-- |Return a'Time' value as a number of microseconds.
toMicroseconds :: Integral a => Time -> a
toMicroseconds (Time t) = fromIntegral t

-- |Construct a 'Time' value from a number of seconds.
fromSeconds :: RealFrac a => a -> Time
fromSeconds t = Time $ truncate (t * 1000000)

-- |Construct a 'Time' value from a number of milliseconds.
fromMilliseconds :: Integral a => a -> Time
fromMilliseconds t = Time $ fromIntegral t * 1000

-- |Construct a 'Time' value from a number of microseconds.
fromMicroseconds :: Integral a => a -> Time
fromMicroseconds t = Time $ fromIntegral t

