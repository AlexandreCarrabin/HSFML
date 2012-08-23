module SFML.Graphics.Texture 
    ( Texture
    , createTexture
    , createTextureFromFile
    , createTextureFromMemory
    , createTextureFromStream
    , createTextureFromImage
    , copyTexture

    , getSize
    , copyToImage
    , updateFromPixels
    , updateFromImage
    , updateFromWindow
    , updateFromRenderWindow
    , bind
    , setSmooth
    , isSmooth
    , setRepeated
    , isRepeated
    , getMaximumSize
    ) where


import SFML.Graphics.Internal.Texture
import SFML.Graphics.Internal.Types (Texture)

