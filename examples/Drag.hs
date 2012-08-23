{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Paths_hsfml

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import SFML.Graphics.Color
import SFML.Graphics.Drawable
import SFML.Graphics.Sprite
import SFML.Graphics.Rect
import SFML.Graphics.RenderStates
import SFML.Graphics.Texture
import SFML.Graphics.RenderWindow (RenderWindow, createRenderWindow, setTitle, clear, display, pollEvent)

import SFML.System.Clock
import SFML.System.Time
import SFML.System.Vector

import SFML.Window.Event
import SFML.Window.Mouse
import SFML.Window.VideoMode
import SFML.Window.Window (ContextSettings(..), Style(..))


data AppData = AppData
    { frame       :: Int
    , fps         :: Clock
    , updateTimer :: Clock

    , images      :: [Sprite]
    , selected    :: Maybe Sprite
    , offset      :: Vector2D Float
    }

getFrame :: MonadState AppData m => m Int
getFrame = liftM frame get

putFrame :: MonadState AppData m => Int -> m ()
putFrame frm = modify $ \s -> s { frame = frm }

modifyFrame :: MonadState AppData m => (Int -> m Int) -> m ()
modifyFrame act = getFrame >>= act >>= putFrame

getFPS :: MonadState AppData m => m Clock
getFPS = liftM fps get

putFPS :: MonadState AppData m => Clock -> m ()
putFPS clock = modify $ \s -> s { fps = clock }

getUpdate :: MonadState AppData m => m Clock
getUpdate = liftM updateTimer get

putUpdate :: MonadState AppData m => Clock -> m ()
putUpdate clock = modify $ \s -> s { updateTimer = clock }

modifyUpdate :: MonadState AppData m => (Clock -> m Clock) -> m ()
modifyUpdate act = getUpdate >>= act >>= putUpdate

getImages :: MonadState AppData m => m [Sprite]
getImages = liftM images get

putImages :: MonadState AppData m => [Sprite] -> m ()
putImages i = modify $ \s -> s { images = i }

modifyImages :: MonadState AppData m => ([Sprite] -> m [Sprite]) -> m ()
modifyImages act = getImages >>= act >>= putImages

getSelected :: MonadState AppData m => m (Maybe Sprite)
getSelected = liftM selected get

putSelected :: MonadState AppData m => Maybe Sprite -> m ()
putSelected sprite = modify $ \s -> s { selected = sprite }

modifySelected :: MonadState AppData m => (Maybe Sprite -> m (Maybe Sprite)) -> m ()
modifySelected act = getSelected >>= act >>= putSelected

getOffset :: MonadState AppData m => m (Vector2D Float)
getOffset = liftM offset get

putOffset :: MonadState AppData m => Vector2D Float -> m ()
putOffset o = modify $ \s -> s { offset = o }


data AppConfig = AppConfig
    { window :: RenderWindow
    }

getWindow :: MonadReader AppConfig m => m RenderWindow
getWindow = liftM window ask


type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState


colors :: [Color]
colors = [ Color 255 255 255 127, Color   0 255 255 127, Color 255   0 255 127
         , Color 255 255   0 127, Color 255   0   0 127, Color   0 255   0 127
         , Color   0   0 255 127, Color 255 127   0 127, Color   0 127 255 127
         ]

initEnv :: IO (AppConfig, AppData)
initEnv = do
    win <- createRenderWindow mode title styles settings
    
    path <- getDataFileName "resources/sfml.png"

    texture <- createTextureFromFile path emptyRect
    imgs <- forM colors $ \c -> do
        image <- createSprite
        setColor image c
        setTexture image (Just texture) False
        setPosition image (Vector2D 200 200)
        return image

    fpsClock <- createClock
    updateClock <- createClock

    return (AppConfig win, AppData 0 fpsClock updateClock imgs Nothing (Vector2D 0 0))

    where
        mode = VideoMode 800 600 24
        title = "Test HSFML"
        styles = [Titlebar, Close]
        settings = ContextSettings 24 24 0 0 0

loop :: AppEnv ()
loop = do
    win <- getWindow
    imgs <- getImages
    s <- getSelected

    quit <- whileEvents win handleEvent

    frameNb <- fmap (+1) getFrame
    putFrame frameNb

    ticks <- (return . toMilliseconds) =<< (liftIO . getElapsedTime =<< getUpdate) :: AppEnv Integer

    when (ticks > 1000) $ do
        fpsClock <- getFPS
        liftIO $ do
            avgPerSec <- (fromIntegral frameNb /) `fmap` (return . toSeconds =<< getElapsedTime fpsClock) :: IO Double
            setTitle win  ("rate : " ++ show avgPerSec ++ " -- " ++ show ticks)
        modifyUpdate $ liftIO . \_ -> createClock

    liftIO $ do
        clear win sfBlack
        mapM_ (\i -> draw win i defaultRenderStates) imgs
        case s of
            Nothing -> return ()
            Just i  -> draw win i defaultRenderStates
        display win

    unless quit loop

whileEvents :: MonadIO m => RenderWindow -> (Event -> m ()) -> m Bool
whileEvents win act = do
    event <- liftIO (pollEvent win)
    case event of
        Nothing     -> return False
        Just Closed -> return True
        Just e      -> act e >> whileEvents win act

handleEvent :: Event -> AppEnv ()
handleEvent (MouseButtonPressed MouseLeftButton (Vector2D x y)) = 
    selectImageAt (Vector2D (fromIntegral x) (fromIntegral y))
handleEvent (MouseButtonReleased MouseLeftButton _) = 
    unselectImage
handleEvent (MouseMoved (Vector2D x y)) = do
    sprite <- getSelected
    case sprite of
        Nothing -> return ()
        Just s -> do 
            Vector2D x' y' <- getOffset
            liftIO $ setPosition s (Vector2D (fromIntegral x - x') (fromIntegral y - y'))
handleEvent _ = return ()

selectImageAt :: Vector2D Float -> AppEnv ()
selectImageAt position@(Vector2D x y) = do
    imgs <- getImages
    (img, imgs') <- liftIO $ getImageAt (reverse imgs) position
    case img of
        Nothing -> return ()
        Just i  -> do
            putImages imgs'
            putSelected $ Just i
            Vector2D x' y' <- liftIO $ getPosition i
            putOffset (Vector2D (x - x') (y - y'))

    where
        getImageAt xs pos = getImageAt' xs [] pos
        getImageAt' [] acc _       = return (Nothing, acc)
        getImageAt' (e:es) acc pos = liftIO $ do
            rect <- getGlobalBounds e
            if contains rect pos
                then return (Just e, reverse es ++ acc)
                else getImageAt' es (e:acc) pos

unselectImage :: AppEnv ()
unselectImage = do
    image <- getSelected
    case image of
        Nothing -> return ()
        Just i  -> modifyImages (\xs -> return (xs ++ [i]))
    putSelected Nothing


runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main :: IO ()
main = do
    (env, st) <- initEnv
    runLoop env st

