module SFML.Utility.Foreign where

import Control.Applicative ((<$>))
import Data.Bits (Bits, (.|.), shift, testBit)
import Data.ByteString (ByteString, unpack)
import Data.Char (chr, ord)
import Data.Word (Word8)
import Foreign (Ptr, Storable, castPtr, fromBool, nullPtr, peek, toBool)
import Foreign.Marshal.Array (allocaArray, peekArray0, pokeArray, withArray, withArray0)


-- |Converts a value from an Integral instance to another.
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

-- |Extract the value contained in an Integral pointer.
cIntPeek :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
cIntPeek a = peek a >>= return . fromIntegral

-- |Convert a Integral value to a Bool. 0 is False, all other values are True.
cIntToBool :: Integral i => i -> Bool
cIntToBool = toBool

-- |Convert a Bool to an Integral value.
cIntFromBool :: Integral i => Bool -> i
cIntFromBool = fromBool

-- |Convert an Integral value to an Enum value.
cIntToEnum :: (Integral i, Enum e) => i -> e
cIntToEnum = toEnum . fromIntegral

-- |Convert an Enum value to an Integral value.
cIntFromEnum ::(Integral i,  Enum e) => e -> i
cIntFromEnum = fromIntegral . fromEnum

-- |Convert an Integral value to a List of Enum values.
cIntToEnumList :: (Bits i, Integral i, Bounded e, Enum e) => i -> [e]
cIntToEnumList 0  = [cIntToEnum (0::Int)]
cIntToEnumList x = filter (testBit x . pred . fromEnum) $ tail [minBound ..]

-- |Convert a List of Enum values to an Integer value.
cIntFromEnumList :: (Bits i, Integral i, Enum e) => [e] -> i
cIntFromEnumList []     = 0
cIntFromEnumList (x:xs) = 1 `shift` (fromEnum x - 1) .|. cIntFromEnumList xs

cFloatConv :: (Real a, Fractional b) => a -> b
cFloatConv = realToFrac

-- |Extract the value contained in a Real pointer.
cFloatPeek :: (Storable a, Real a, Fractional b) => Ptr a -> IO b
cFloatPeek a = peek a >>= return . realToFrac

-- |Convert a Char to its unicode value.
toCodePoint :: Integral i => Char -> i
toCodePoint = cIntConv . ord

withObjectList :: (Storable a, Integral b) => [a] -> ((Ptr a, b) -> IO c) -> IO c
withObjectList xs act = do
    let len = length xs
    allocaArray len $ \ptr -> do
        pokeArray ptr xs
        act (ptr, (cIntConv len))

withByteString :: (Integral i, Storable i) => ByteString -> (Ptr i -> IO a) -> IO a
withByteString b = withArray $ map cIntConv (unpack b)

withByteStringLen :: (Storable a, Integral a, Integral b) => ByteString -> ((Ptr a, b) -> IO c) -> IO c
withByteStringLen b act = do
    let bytes = (map cIntConv . unpack) b
    withObjectList bytes act

withVoidPtr :: Integral a => ByteString -> ((Ptr (), a) -> IO b) -> IO b
withVoidPtr b act = withByteStringLen b (\(ptr, len) -> act ((castPtr (ptr :: Ptr Word8)), len))

-- |Returns an unicode String from a Integral 0-terminated array pointer.
peekUnicodeString :: (Integral i, Storable i) => Ptr i -> IO String
peekUnicodeString s = peekArray0 0 s >>= return . map (chr . cIntConv)

withUnicodeString :: (Integral i, Storable i) => String -> (Ptr i -> IO a) -> IO a
withUnicodeString s = withArray0 0 $ map (cIntConv . ord) s

ptrToMaybe :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
ptrToMaybe act ptr = 
    if ptr == nullPtr
        then return Nothing
        else Just <$> act ptr

maybeToPtr :: Maybe a -> (a -> (Ptr b -> IO c) -> IO c) -> (Ptr b -> IO c) -> IO c
maybeToPtr Nothing  _     act = act nullPtr
maybeToPtr (Just m) withM act = withM m act

