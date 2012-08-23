{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.InputStream where


import Prelude hiding (read)

import Control.Applicative ((<$>))
import Data.ByteString (ByteString, unpack)
import Foreign (ForeignPtr, Ptr, pokeArray, withForeignPtr)
--import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CChar, CLLong)
import Foreign.Concurrent (newForeignPtr)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

import SFML.Utility.Foreign (cIntConv)

#include <SFML/System/InputStream.h>
#include <SFML/System/InputStreamWrapper.h>


type CSFML_InputStream = ()
{# pointer *sfInputStream as InputStreamPtr -> CSFML_InputStream #}
data InputStream a = InputStream (ForeignPtr CSFML_InputStream)
                                 (StablePtr (InputStreamCallbackData a))

withInputStream :: InputStream a -> (InputStreamPtr -> IO b) -> IO b
withInputStream (InputStream is _) = withForeignPtr is


-- |Callback to read data from the stream.
type CallbackRead a i = a -> i -> IO (ByteString, i)
-- |Callback to seek a position in the stream.
type CallbackSeek a i = a -> i -> IO i
-- |Callback to tell the current position in the stream.
type CallbackTell a i = a -> IO i
-- |Callback to get the total size of the stream.
type CallbackSize a i = a -> IO i

data InputStreamCallbackData a =
    InputStreamCallbackData a
                            (a -> CLLong -> IO (ByteString, CLLong))
                            (a -> CLLong -> IO CLLong)
                            (a -> IO CLLong)
                            (a -> IO CLLong)

-- |Create a custom `InputStream'.
createInputStream :: 
    Integral i => a -> CallbackRead a i -> CallbackSeek a i -> CallbackTell a i -> CallbackSize a i -> IO (InputStream a)
createInputStream d callbackRead callbackSeek callbackTell callbackSize =
    createInputStreamCallbackData d callbackRead callbackSeek callbackTell callbackSize >>= \dataPtr ->
    {# call unsafe sfInputStream_create_wrapper #} (castStablePtrToPtr dataPtr) >>= \c_inputStream ->
    newForeignPtr c_inputStream (finalizeInputStream c_inputStream dataPtr) >>= \inputStreamPtr ->
    return $ InputStream inputStreamPtr dataPtr

finalizeInputStream :: InputStreamPtr -> StablePtr a -> IO ()
finalizeInputStream inputStream callbackData = do
    {# call unsafe sfInputStream_destroy_wrapper #} inputStream
    freeStablePtr callbackData


createInputStreamCallbackData :: 
    Integral i => a -> CallbackRead a i -> CallbackSeek a i -> CallbackTell a i -> CallbackSize a i -> IO (StablePtr (InputStreamCallbackData a))
createInputStreamCallbackData d callbackRead callbackSeek callbackTell callbackSize =
    newStablePtr $
        InputStreamCallbackData d 
                                (wrapCallbackRead callbackRead)
                                (wrapCallbackSeek callbackSeek)
                                (wrapCallbackTell callbackTell)
                                (wrapCallbackSize callbackSize)

wrapCallbackRead :: Integral i => CallbackRead a i -> (a -> CLLong -> IO (ByteString, CLLong))
wrapCallbackRead fn = \d readSize -> 
     fn d (cIntConv readSize) >>= \(bytes, actualReadSize) ->
     return (bytes, cIntConv actualReadSize)

wrapCallbackSeek :: Integral i => CallbackSeek a i -> (a -> CLLong -> IO CLLong)
wrapCallbackSeek fn = \d position -> cIntConv <$> fn d (cIntConv position)

wrapCallbackTell :: Integral i => CallbackTell a i -> (a -> IO CLLong)
wrapCallbackTell fn = \d -> cIntConv <$> fn d

wrapCallbackSize :: Integral i => CallbackSize a i -> (a -> IO CLLong)
wrapCallbackSize fn = \d -> cIntConv <$> fn d


-- |Read data from the stream.
foreign export ccall hs_readCallback :: Ptr CChar -> CLLong -> StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_readCallback :: Ptr CChar -> CLLong -> StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_readCallback result readSize value =
    deRefStablePtr value >>= \(InputStreamCallbackData d callbackRead _ _ _) ->
    callbackRead d readSize >>= \(bytes, actualReadSize) ->
    pokeArray result ((map cIntConv . unpack) bytes) >>
    --pokeArray result (map castCharToCChar bytes) >>
    return actualReadSize

-- |Change the current reading position.
foreign export ccall hs_seekCallback :: CLLong -> StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_seekCallback :: CLLong -> StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_seekCallback position value =
    deRefStablePtr value >>= \(InputStreamCallbackData d _ callbackSeek _ _) ->
    callbackSeek d position

-- |Get the current reading position in the stream.
foreign export ccall hs_tellCallback :: StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_tellCallback :: StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_tellCallback value =
    deRefStablePtr value >>= \(InputStreamCallbackData d _ _ callbackTell _) ->
    callbackTell d

-- |Return the size of the stream.
foreign export ccall hs_getSizeCallback :: StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_getSizeCallback :: StablePtr (InputStreamCallbackData a) -> IO CLLong
hs_getSizeCallback value = 
    deRefStablePtr value >>= \(InputStreamCallbackData d _ _ _ callbackSize) ->
    callbackSize d

