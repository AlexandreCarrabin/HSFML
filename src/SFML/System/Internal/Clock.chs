{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Clock where


import Control.Applicative ((<$>))
import Foreign (FinalizerPtr, newForeignPtr, alloca, peek)

{# import SFML.System.Internal.Time #} (Time(..), TimePtr)
{# import SFML.System.Internal.Types #} (Clock(..), ClockPtr, CSFML_Clock, withClock)

#include <SFML/System/Clock.h>
#include <SFML/System/ClockWrapper.h>


-- |Create a new 'Clock' and start it.
createClock :: IO Clock
createClock =
    {# call unsafe sfClock_create #} >>= \res ->
    mkClock res

mkClock :: ClockPtr -> IO Clock
mkClock ptr = Clock <$> newForeignPtr c_destroyClock ptr

-- |Create a new 'Clock' by copying an existing one.
copyClock :: Clock -> IO Clock
copyClock clock =
    withClock clock $ \ptr ->
    {# call unsafe sfClock_copy #} ptr >>= \res ->
    mkClock res

-- |Destroy a 'Clock'.
foreign import ccall unsafe "SFML/System/Clock.h &sfClock_destroy"
    c_destroyClock :: FinalizerPtr CSFML_Clock

-- |Returns the time elapsed since the last call to 'restart' 
-- (or the creation of the object if 'restart' has not been called).
getElapsedTime :: Clock -> IO Time
getElapsedTime clock =
    withClock clock $ \ptr ->
    alloca $ \time ->
    {# call unsafe sfClock_getElapsedTime_wrapper #} ptr time >>
    peek time

-- |Puts the time counter back to zero.
-- It also returns the time elapsed since the 'Clock' was started.
restart :: Clock -> IO Time
restart clock =
    withClock clock $ \ptr ->
    alloca $ \time ->
    {# call unsafe sfClock_restart_wrapper #} ptr time >>
    peek time

