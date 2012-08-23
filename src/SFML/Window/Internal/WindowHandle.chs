{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.WindowHandle where


import Foreign.C.Types (CULong)

#include <SFML/Window/WindowHandle.h>


--FIXME: Seperate Linux, OSX, Windows ...

type WindowHandle = {# type sfWindowHandle #}

withWindowHandle :: WindowHandle -> CULong
withWindowHandle = id

