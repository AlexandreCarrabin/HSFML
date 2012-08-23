{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Sleep where


import Foreign (with)

{# import SFML.System.Internal.Time #} (Time, TimePtr)


#include <SFML/System/Sleep.h>
#include <SFML/System/SleepWrapper.h>


-- |Make the current thread sleep for a given duration.
sleep :: Time -> IO ()
sleep time =
    with time $ \ptr ->
    {# call unsafe sfSleep_wrapper #} ptr

