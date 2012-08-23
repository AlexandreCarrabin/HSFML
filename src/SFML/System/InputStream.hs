module SFML.System.InputStream 
    ( InputStream
    , createInputStream

    , CallbackRead
    , CallbackSeek
    , CallbackTell
    , CallbackSize

    , InputStreamData(..)
    , createInputStreamWithData
    ) where


import Prelude hiding (read)

import Control.Applicative ((<$>))
import Data.ByteString (packCStringLen)
import Foreign (allocaBytes)
--import Foreign.C.String (peekCStringLen)
import System.IO (Handle, SeekMode(..), hFileSize, hGetBuf, hSeek, hTell)

import SFML.System.Internal.InputStream

import SFML.Utility.Foreign (cIntConv)


class InputStreamData a where
    read :: Integral i => CallbackRead a i
    seek :: Integral i => CallbackSeek a i
    tell :: Integral i => CallbackTell a i
    size :: Integral i => CallbackSize a i

-- |This instance is only valid for `Handle' for which the function `hFileSize' is valid
--(ie handle attached to a physical file)
instance InputStreamData Handle where
    read h i = 
        allocaBytes (cIntConv i) $ \cstring ->
        hGetBuf h cstring (cIntConv i) >>= \actualReadSize ->
        packCStringLen (cstring, actualReadSize) >>= \readBytes ->
        --peekCStringLen (cstring, actualReadSize) >>= \readBytes ->
        return (readBytes, cIntConv actualReadSize)
    seek h i = hSeek h AbsoluteSeek (cIntConv i) >> tell h
    tell h   = cIntConv <$> hTell h
    size h   = cIntConv <$> hFileSize h


-- |Create a custom `InputStream' from an instance of `InputStreamData'.
createInputStreamWithData :: InputStreamData a => a -> IO (InputStream a)
createInputStreamWithData streamData = createInputStream streamData read seek tell size

