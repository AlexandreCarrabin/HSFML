{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Types 
    ( Clock(..)
    , CSFML_Clock
    , ClockPtr
    , withClock

    , Mutex(..)
    , CSFML_Mutex
    , MutexPtr
    , withMutex

    , Thread(..)
    , CSFML_Thread
    , ThreadPtr
    , withThread
    ) where


import Foreign (ForeignPtr, Ptr, withForeignPtr) 

#include <SFML/System/Types.h>


type CSFML_Clock = ()
{# pointer *sfClock as ClockPtr -> CSFML_Clock #}
data Clock = Clock (ForeignPtr CSFML_Clock)

withClock :: Clock -> (ClockPtr -> IO a) -> IO a
withClock (Clock c) = withForeignPtr c


type CSFML_Mutex = ()
{# pointer *sfMutex as MutexPtr -> CSFML_Mutex #}
data Mutex = Mutex (ForeignPtr CSFML_Mutex)

withMutex :: Mutex -> (MutexPtr -> IO a) -> IO a
withMutex (Mutex m) = withForeignPtr m


type CSFML_Thread = ()
{# pointer *sfThread as ThreadPtr -> CSFML_Thread #}
data Thread = Thread (ForeignPtr CSFML_Thread) 

withThread :: Thread -> (ThreadPtr -> IO a) -> IO a
withThread (Thread t) = withForeignPtr t

