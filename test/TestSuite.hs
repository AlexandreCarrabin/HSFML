module Main where


import Test.Framework (defaultMain, testGroup)

import qualified SFML.Graphics.Color.Tests
import qualified SFML.Graphics.Rect.Tests
import qualified SFML.System.Time.Tests


main :: IO ()
main = defaultMain tests
    where
        tests = [ testGroup "SFML.Graphics.Color.Tests"
                            SFML.Graphics.Color.Tests.tests
                , testGroup "SFML.Graphics.Rect.Tests"
                            SFML.Graphics.Rect.Tests.tests
                , testGroup "SFML.System.Time.Tests"
                            SFML.System.Time.Tests.tests
                ]

