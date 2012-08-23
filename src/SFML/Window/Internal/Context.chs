{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Window.Internal.Context where


import Control.Applicative ((<$>))
import Foreign (FinalizerPtr, newForeignPtr)
import Foreign.C.Types (CInt)

{# import SFML.Window.Internal.Types #} (CSFML_Context, Context(..), ContextPtr, withContext)

import SFML.Utility.Foreign (cIntFromBool)

#include <SFML/Window/Context.h>


-- |Create a new `Context'. This function activates the `Context'.
createContext :: IO Context
createContext = {# call unsafe sfContext_create #} >>= mkContext

mkContext :: ContextPtr -> IO Context
mkContext ptr = Context <$> newForeignPtr c_destroyContext ptr

-- |Destroy a `Context'.
foreign import ccall unsafe "SFML/Window/Context.h &sfContext_destroy"
    c_destroyContext :: FinalizerPtr CSFML_Context

-- |Activate or deactivate explicitely a `Context'.
setActive :: Context -> Bool -> IO ()
setActive context active =
    withContext context $ \c ->
    {# call unsafe sfContext_setActive #} c (cIntFromBool active)

