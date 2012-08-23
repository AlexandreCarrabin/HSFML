{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad (when)

import Reactive.Banana

import SFML.Graphics.Circle as C
import SFML.Graphics.Color
import SFML.Graphics.Drawable
import SFML.Graphics.RenderStates
import SFML.Graphics.RenderWindow
import SFML.System.Clock
import SFML.System.Sleep
import SFML.System.Time
import SFML.System.Vector
import SFML.Window.Event as S
import SFML.Window.Keyboard
import SFML.Window.Mouse
import SFML.Window.VideoMode
import SFML.Window.Window (ContextSettings(..), Style(..))


type Source a = (AddHandler a, a -> IO ())
type EventSource = Source S.Event
type TickSource  = Source ()

data MouseClick = Up (Vector2D Float)
                | Down (Vector2D Float)

addHandler :: (AddHandler a, a -> IO ()) -> AddHandler a
addHandler = fst

fire :: (AddHandler a, a -> IO ()) -> a -> IO ()
fire = snd


main :: IO ()
main = do
    window <- createRenderWindow mode title styles settings

    sources <- (,) <$> newAddHandler <*> newAddHandler
    network <- compile $ setupNetwork window sources
    actuate network

    eventLoopAtFramerate window 50 sources
    --eventLoop window sources network

    where
        mode = VideoMode 800 600 24
        title = "Test HSFML"
        styles = [Titlebar, Close]
        settings = ContextSettings 24 24 0 0 0


eventLoopAtFramerate :: RenderWindow -> Int -> (EventSource, TickSource) -> IO ()
eventLoopAtFramerate window fps (esource, tsource) = do
    clock <- createClock
    loop clock

    where
        loop c = do
            _ <- restart c
            continue <- eventLoop' window esource
            t <- return . toSeconds =<< getElapsedTime c
            let wait = 1 / (fromIntegral fps) - t :: Double
            when (wait > 0) $ sleep (fromSeconds wait)
            fire tsource ()
            when continue $ loop c

eventLoop :: RenderWindow -> (EventSource, TickSource) -> IO ()
eventLoop window (esource, tsource) = do
    continue <- eventLoop' window esource
    fire tsource ()
    when continue $ eventLoop window (esource, tsource)

eventLoop' :: RenderWindow -> EventSource -> IO Bool
eventLoop' window esource = getEvent
    where
        getEvent :: IO Bool
        getEvent = do
            me <- pollEvent window
            case me of
                Just Closed -> return False
                Just e      -> fire esource e >> getEvent
                Nothing     -> return True


setupNetwork :: forall t. RenderWindow -> (EventSource, TickSource) -> NetworkDescription t ()
setupNetwork window (esource, tsource) = do

    -- TickEvent
    etick <- fromAddHandler (addHandler tsource)

    -- SFML events
    esfml <- fromAddHandler (addHandler esource)

    let ekey   = filterE onlyKeys esfml
        bpoint = accumB (Vector2D 400 300) $ movePoint . extractCode <$> ekey

        eMouse     = filterE filterMouseClickEvent esfml
        eMousePos  = filterE mouseMove esfml
        
        eMouseDown = filterE isMouseDown eMouse
        eMouseUp   = filterE isMouseUp   eMouse
        bInPos = isInCircle <$> bMousePos <*> bMouseMove

        bDrag  = stepper False $ (bInPos <@ eMouseDown) `union` (fmap (\_ -> False) eMouseUp)
        eDrag  = whenE bDrag eMousePos

        bMouseMove = stepper (Vector2D 0 0) $ extractPos <$> filterE mouseMove esfml
        bMousePos  = accumB (Vector2D 100 100) $ ((.++.) <$> (bmove <@ eDrag))

        bpos = stepper (Vector2D 0 0) $ extractPos <$> eMousePos
        bmove = stepper (Vector2D 0 0) $ computeMove <$> bpos <@> (extractPos <$> eMousePos)

        edraw  = (,) <$> bpoint <*> bMousePos <@ etick
    
    reactimate $ fmap (drawScreen window) edraw

    where
        onlyKeys :: S.Event -> Bool
        onlyKeys (KeyPressed _ _) = True
        onlyKeys _                = False

        extractCode :: S.Event -> KeyCode
        extractCode (KeyPressed code _) = code
        extractCode _ = error "Undefined Event"

        movePoint :: KeyCode -> Vector2D Float -> Vector2D Float
        movePoint k pos = case k of
                            KeyUp    -> pos .++. (Vector2D   0 (-3))
                            KeyDown  -> pos .++. (Vector2D   0   3 )
                            KeyLeft  -> pos .++. (Vector2D (-3)  0 )
                            KeyRight -> pos .++. (Vector2D   3   0 )
                            _        -> pos

        filterMouseClickEvent :: S.Event -> Bool
        filterMouseClickEvent (MouseButtonPressed  MouseLeftButton _) = True
        filterMouseClickEvent (MouseButtonReleased MouseLeftButton _) = True
        filterMouseClickEvent _ = False

        mouseMove :: S.Event -> Bool
        mouseMove (MouseMoved _) = True
        mouseMove _ = False

        extractPos :: S.Event -> Vector2D Float
        extractPos (MouseButtonPressed  _ (Vector2D x y)) = Vector2D (fromIntegral x) (fromIntegral y)
        extractPos (MouseButtonReleased _ (Vector2D x y)) = Vector2D (fromIntegral x) (fromIntegral y)
        extractPos (MouseMoved            (Vector2D x y)) = Vector2D (fromIntegral x) (fromIntegral y)
        extractPos _ = error "Undefined Event"

        isMouseDown :: S.Event -> Bool
        isMouseDown (MouseButtonPressed  _ _) = True
        isMouseDown (MouseButtonReleased _ _) = False
        isMouseDown _ = error "Undefined Event"

        isMouseUp :: S.Event -> Bool
        isMouseUp (MouseButtonPressed  _ _) = False
        isMouseUp (MouseButtonReleased _ _) = True
        isMouseUp _ = error "Undefined Event"

        isInCircle :: Vector2D Float -> Vector2D Float -> Bool
        isInCircle (Vector2D cx cy) (Vector2D mx my) = mx >= cx - 10 && mx <= cx + 10 && my >= cy - 10 && my <= cy + 10 

        computeMove :: Vector2D Float -> Vector2D Float -> Vector2D Float
        computeMove old pos = pos .--. old

drawScreen :: RenderWindow -> (Vector2D Float, Vector2D Float) -> IO ()
drawScreen window (pos1, pos2) = do
    clear window sfBlack

    circle1 <- createCircle
    C.setPosition circle1 pos1
    C.setRadius circle1 10
    C.setFillColor circle1 sfRed
    draw window circle1 defaultRenderStates

    circle2 <- createCircle
    C.setPosition circle2 (pos2 .--. (Vector2D 10 10))
    C.setRadius circle2 10
    C.setFillColor circle2 sfBlue
    draw window circle2 defaultRenderStates

    display window

