module SFML.Graphics.Rect.Tests
    ( tests
    ) where


import Control.Applicative ((<$>), (<*>))
import Data.Int (Int32)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, arbitrary, suchThat)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, run)

import SFML.Graphics.Internal.Rect
import SFML.System.Vector (Vector2D(..))


instance Arbitrary a => Arbitrary (Vector2D a) where
    arbitrary = Vector2D <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Rect a) where
    arbitrary = Rect <$> arbitrary <*> arbitrary


tests :: [Test]
tests = [ testProperty "intRectContains" prop_intRectContains
        , testProperty "intRectIntersects" prop_intRectIntersects
        , testProperty "floatRectContains" prop_floatRectContains
        , testProperty "floatRectIntersects" prop_floatRectIntersects
        ]

prop_intRectContains :: Property
prop_intRectContains = monadicIO $ do
    r1 <- pick arbitrary :: PropertyM IO (Rect Int32)
    r2 <- pick arbitrary
    let r3 = rectContains r1 r2
    r4 <- run $ intRectContains r1 r2
    assert $ r3 == r4

prop_intRectIntersects :: Property
prop_intRectIntersects = monadicIO $ do
    r1 <- pick arbitrary :: PropertyM IO (Rect Int32)
    r2 <- pick arbitrary
    let r3 = rectIntersects r1 r2
    r4 <- run $ intRectIntersects r1 r2
    assert $ r3 == r4

prop_floatRectContains :: Property
prop_floatRectContains = monadicIO $ do
    r1 <- pick arbitrary :: PropertyM IO (Rect Float)
    r2 <- pick arbitrary
    let r3 = rectContains r1 r2
    r4 <- run $ floatRectContains r1 r2
    assert $ r3 == r4

prop_floatRectIntersects :: Property
prop_floatRectIntersects = monadicIO $ do
    r1 <- pick arbitrary :: PropertyM IO (Rect Float)
    r2 <- pick arbitrary
    let r3 = rectIntersects r1 r2
    r4 <- run $ floatRectIntersects r1 r2
    assert $ r3 == r4

