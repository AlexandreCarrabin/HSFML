{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Types 
    ( Context(..)
    , CSFML_Context
    , ContextPtr
    , withContext

    , Window(..)
    , CSFML_Window
    , WindowPtr
    , withWindow
    ) where


import Foreign (ForeignPtr, Ptr, withForeignPtr) 

#include <SFML/Window/Types.h>


type CSFML_Context = ()
{# pointer *sfContext as ContextPtr -> CSFML_Context #}
data Context = Context (ForeignPtr CSFML_Context)

withContext :: Context -> (ContextPtr -> IO a) -> IO a
withContext (Context c) = withForeignPtr c


type CSFML_Window = ()
{# pointer *sfWindow as WindowPtr -> CSFML_Window #}
data Window = Window (ForeignPtr CSFML_Window)

withWindow :: Window -> (WindowPtr -> IO a) -> IO a
withWindow (Window w) = withForeignPtr w

