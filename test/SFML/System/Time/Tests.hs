module SFML.System.Time.Tests
    ( tests
    ) where


import Control.Applicative ((<$>))
import Data.Int (Int32, Int64)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, Property, arbitrary)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, run)

import qualified SFML.System.Time as T
import qualified SFML.System.Internal.Time as IT


instance Arbitrary IT.Time where
    arbitrary = IT.Time <$> (fromIntegral <$> (arbitrary :: Gen Int64))


tests :: [Test]
tests = [ testProperty "toSeconds" prop_toSeconds
        , testProperty "toMilliseconds" prop_toMilliseconds
        , testProperty "toMicroseconds" prop_toMicroseconds
        , testProperty "fromSeconds" prop_fromSeconds
        , testProperty "fromMilliseconds" prop_fromMilliseconds
        , testProperty "fromMicroseconds" prop_fromMicroseconds
        ]


prop_toSeconds :: Property
prop_toSeconds = monadicIO $ do
    t <- pick arbitrary
    let s1 = T.toSeconds t :: Float
    s2 <- run $ IT.toSeconds t
    assert $ s1 == s2

prop_toMilliseconds :: Property
prop_toMilliseconds = monadicIO $ do
    t <- pick arbitrary
    let ms1 = T.toMilliseconds t :: Int32
    ms2 <- run $ IT.toMilliseconds t
    assert $ ms1 == ms2

prop_toMicroseconds :: Property
prop_toMicroseconds = monadicIO $ do
    t <- pick arbitrary
    let ms1 = T.toMicroseconds t :: Int64
    ms2 <- run $ IT.toMicroseconds t
    assert $ ms1 == ms2

prop_fromSeconds :: Property
prop_fromSeconds = monadicIO $ do
    s <- pick arbitrary :: PropertyM IO Float
    let t1 = T.fromSeconds s
    t2 <- run $ IT.fromSeconds s
    assert $ t1 == t2

prop_fromMilliseconds :: Property
prop_fromMilliseconds = monadicIO $ do
    s <- pick arbitrary :: PropertyM IO Int32
    let t1 = T.fromMilliseconds s
    t2 <- run $ IT.fromMilliseconds s
    assert $ t1 == t2

prop_fromMicroseconds :: Property
prop_fromMicroseconds = monadicIO $ do
    s <- pick arbitrary :: PropertyM IO Int64
    let t1 = T.fromMicroseconds s
    t2 <- run $ IT.fromMicroseconds s
    assert $ t1 == t2

