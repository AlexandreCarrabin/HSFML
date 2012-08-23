module SFML.Graphics.Color.Tests
    ( tests
    ) where


import Control.Applicative ((<$>), (<*>))
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import qualified SFML.Graphics.Color as C
import qualified SFML.Graphics.Internal.Color as IC


instance Arbitrary C.Color where
    arbitrary = C.Color <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


tests :: [Test]
tests = [ testProperty "fromRGB" prop_fromRGB
        , testProperty "fromRGBA" prop_fromRGBA
        , testProperty "add" prop_add
        , testProperty "modulate" prop_modulate
        ]

prop_fromRGB :: Property
prop_fromRGB = monadicIO $ do
    (r, g, b) <- pick arbitrary
    let c = C.fromRGB r g b
    ic <- run $ IC.fromRGB r g b
    assert $ c == ic


prop_fromRGBA :: Property
prop_fromRGBA = monadicIO $ do
    (r, g, b, a) <- pick arbitrary
    let c = C.fromRGBA r g b a
    ic <- run $ IC.fromRGBA r g b a
    assert $ c == ic

prop_add :: Property
prop_add = monadicIO $ do
    c1 <- pick arbitrary
    c2 <- pick arbitrary
    let c3 = C.add c1 c2
    c4 <- run $ IC.add c1 c2
    assert $ c3 == c4

prop_modulate :: Property
prop_modulate = monadicIO $ do
    c1 <- pick arbitrary
    c2 <- pick arbitrary
    let c3 = C.modulate c1 c2
    c4 <- run $ IC.modulate c1 c2
    assert $ c3 == c4

