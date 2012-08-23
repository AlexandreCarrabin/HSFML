module SFML.Graphics.RenderStates 
    ( RenderStates(..)

    , defaultRenderStates
    ) where


import SFML.Graphics.Internal.BlendMode (BlendMode(..))
import SFML.Graphics.Internal.RenderStates (RenderStates(..))
import SFML.Graphics.Transform (identityTransform)


-- |Get the default `RenderStates'.
-- 
-- >>> defaultRenderStates = RenderStates BlendNone identityTransform Nothing Nothing
--
defaultRenderStates :: RenderStates
defaultRenderStates = RenderStates BlendNone identityTransform Nothing Nothing

