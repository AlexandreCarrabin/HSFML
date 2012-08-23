{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Paths_hsfml

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Random (randomIO)

import SFML.Graphics.Circle (Circle, createCircle, setRadius)
import SFML.Graphics.Color
import SFML.Graphics.Drawable
import SFML.Graphics.Font
import SFML.Graphics.Rectangle (Rectangle, createRectangle, setSize)
import SFML.Graphics.RenderStates
import SFML.Graphics.ShapeLike
import SFML.Graphics.Text (Text, createText, setCharacterSize, setColor, setFont, setString)
import SFML.Graphics.TransformableLike
import SFML.Graphics.RenderWindow (RenderWindow, createRenderWindow, clear, display, pollEvent, setVerticalSyncEnabled)

import SFML.System.Clock
import SFML.System.Time
import SFML.System.Vector

import SFML.Window.Event
import SFML.Window.Keyboard
import SFML.Window.VideoMode
import SFML.Window.Window (ContextSettings(..), Style(..))


data AppData = AppData
    { running     :: Bool
    , ballAngle   :: Float
    , rightPaddleSpeed :: Float
    }

getRunning :: MonadState AppData m => m Bool
getRunning = liftM running get

putRunning :: MonadState AppData m => Bool -> m ()
putRunning status = modify $ \s -> s { running = status }

getBallAngle :: MonadState AppData m => m Float
getBallAngle = liftM ballAngle get

putBallAngle :: MonadState AppData m => Float -> m ()
putBallAngle angle = modify $ \s -> s { ballAngle = angle }

modifyBallAngle :: MonadState AppData m => (Float -> m Float) -> m ()
modifyBallAngle act = getBallAngle >>= act >>= putBallAngle

getRightPaddleSpeed :: MonadState AppData m => m Float
getRightPaddleSpeed = liftM rightPaddleSpeed get

putRightPaddleSpeed :: MonadState AppData m => Float -> m ()
putRightPaddleSpeed speed = modify $ \s -> s { rightPaddleSpeed = speed }

modifyRightPaddleSpeed :: MonadState AppData m => (Float -> m Float) -> m ()
modifyRightPaddleSpeed act = getRightPaddleSpeed >>= act >>= putRightPaddleSpeed


data AppConfig = AppConfig
    { window :: RenderWindow
    , msg    :: Text

    , leftPaddle :: Rectangle
    , rightPaddle :: Rectangle
    , ball :: Circle

    , timer :: Clock
    , aiTimer :: Clock
    }

getWindow :: MonadReader AppConfig m => m RenderWindow
getWindow = liftM window ask

getMsg :: MonadReader AppConfig m => m Text
getMsg = liftM msg ask

getLeftPaddle :: MonadReader AppConfig m => m Rectangle
getLeftPaddle = liftM leftPaddle ask

getRightPaddle :: MonadReader AppConfig m => m Rectangle
getRightPaddle = liftM rightPaddle ask

getBall :: MonadReader AppConfig m => m Circle
getBall = liftM ball ask

getTimer :: MonadReader AppConfig m => m Clock
getTimer = liftM timer ask

getAiTimer :: MonadReader AppConfig m => m Clock
getAiTimer = liftM aiTimer ask


type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState


gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 600

paddleSize :: Vector2D Float
paddleSize = Vector2D 25 100

paddleSpeed :: Float
paddleSpeed = 400

ballSpeed :: Float
ballSpeed = 400

ballRadius :: Float
ballRadius = 10

aiTime :: Float
aiTime = 0.1

initEnv :: IO (AppConfig, AppData)
initEnv = do
    w <- createRenderWindow mode title styles settings
    setVerticalSyncEnabled w True

    let Vector2D xp yp = paddleSize
    let paddleColor = fromRGB 100 100 200
    
    --Create the left paddle
    lp <- createRectangle
    setSize lp (Vector2D (xp - 3) (yp - 3))
    setOutlineThickness lp 3
    setOutlineColor lp sfBlack
    setFillColor lp paddleColor
    setOrigin lp (Vector2D (xp / 2) (yp / 2))

    --Create the right paddle
    rp <- createRectangle
    setSize rp (Vector2D (xp - 3) (yp - 3))
    setOutlineThickness rp 3
    setOutlineColor rp sfBlack
    setFillColor rp paddleColor
    setOrigin rp (Vector2D (xp / 2) (yp / 2))

    --Create the ball
    b <- createCircle
    setRadius b (ballRadius - 3)
    setOutlineThickness b 3
    setOutlineColor b sfBlack
    setFillColor b sfWhite
    setOrigin b (Vector2D (ballRadius / 2) (ballRadius / 2))

    --Create clocks
    t <- createClock
    ai <- createClock

    --Load the font
    font_path <- getDataFileName "resources/sansation.ttf"
    font <- createFontFromFile font_path

    --Create the pause message
    pauseMsg <- createText
    setFont pauseMsg (Just font)
    setCharacterSize pauseMsg 40
    setPosition pauseMsg (Vector2D 170 150)
    setColor pauseMsg sfWhite
    setString pauseMsg "Welcome to SFML pong!\nPress space to start the game"

    return (AppConfig w pauseMsg lp rp b t ai, AppData False 0 0)

    where
        mode = VideoMode (fromIntegral gameWidth) (fromIntegral gameHeight) 24
        title = "Pong HSFML"
        styles = [Titlebar, Close]
        settings = ContextSettings 24 24 0 0 0

loop :: AppEnv ()
loop = do
    w <- getWindow
    m <- getMsg

    status <- getRunning
    lp <- getLeftPaddle
    rp <- getRightPaddle
    b <- getBall

    let background = fromRGB 50 200 50

    quit <- whileEvents w handleEvent

    when status updatePong

    liftIO $ do
        clear w background
        if status
            then do
                draw w lp defaultRenderStates
                draw w rp defaultRenderStates
                draw w b  defaultRenderStates
            else 
                draw w m defaultRenderStates
        display w

    unless quit loop

whileEvents :: MonadIO m => RenderWindow -> (Event -> m ()) -> m Bool
whileEvents win act = do
    event <- liftIO (pollEvent win)
    case event of
        Nothing     -> return False
        Just Closed -> return True
        Just (KeyPressed KeyEscape _) -> return True
        Just e      -> act e >> whileEvents win act

handleEvent :: Event -> AppEnv ()
handleEvent (KeyPressed KeySpace _) = do
    status <- getRunning
    case status of
        False -> startGame
        True  -> return ()
handleEvent _ = return ()

startGame :: AppEnv ()
startGame = do
    let Vector2D xp _ = paddleSize

    lp <- getLeftPaddle
    liftIO $ setPosition lp (Vector2D (10 + xp / 2) (fromIntegral gameHeight / 2))

    rp <- getRightPaddle
    liftIO $ setPosition rp (Vector2D (fromIntegral gameWidth - 10 - xp / 2) (fromIntegral gameHeight / 2))

    b <- getBall
    liftIO $ setPosition b (Vector2D (fromIntegral gameWidth / 2) (fromIntegral gameHeight / 2))

    t <- getTimer
    _ <- liftIO $ restart t

    liftIO initBallAngle >>= putBallAngle
    putRunning True


initBallAngle :: IO Float
initBallAngle = do
    r <- randomIO
    let angle = 2 * pi * r
    if abs (cos angle) < 0.7
        then initBallAngle
        else return angle


updatePong :: AppEnv ()
updatePong = do
    deltaTime <- return . toSeconds =<< liftIO . restart =<< getTimer
    let Vector2D xp yp = paddleSize
    b <- getBall
    Vector2D xb yb <- liftIO $ getPosition b
    angle <- getBallAngle

    --Move the player's paddle
    lp <- getLeftPaddle
    Vector2D xl yl <- liftIO $ getPosition lp

    isUp <- liftIO $ isKeyPressed KeyUp
    when (isUp && yl - yp / 2 > 5) $
        liftIO $ move lp (Vector2D 0 (-paddleSpeed * deltaTime))

    isDown <- liftIO $ isKeyPressed KeyDown
    when (isDown && yl + yp / 2 < fromIntegral gameHeight - 5) $
        liftIO $ move lp (Vector2D 0 (paddleSpeed * deltaTime))

    --Move the computer's paddle
    rp <- getRightPaddle
    Vector2D xr yr <- liftIO $ getPosition rp
    rps <- getRightPaddleSpeed
    when ((rps < 0 && yr - yp / 2 > 5) || (rps > 0 && yr + yp / 2 < fromIntegral gameHeight - 5)) $
        liftIO $ move rp (Vector2D 0 (rps * deltaTime))

    --Update the computer's paddle direction according to the ball position
    deltaAiTime <- (return . toSeconds) =<< (liftIO . getElapsedTime =<< getAiTimer)
    when (deltaAiTime > aiTime) $ do
        _ <- liftIO . restart =<< getAiTimer
        if yb + ballRadius > yr + yp / 2
            then putRightPaddleSpeed paddleSpeed
            else if yb - ballRadius < yr -yp / 2
                then putRightPaddleSpeed (-paddleSpeed)
                else putRightPaddleSpeed 0

    --Move the ball
    let factor = ballSpeed * deltaTime
    liftIO $ move b (Vector2D (cos angle * factor) (sin angle * factor))

    --Check collisions between the ball and the screen
    m <- getMsg
    when (xb - ballRadius < 0) $ do
        putRunning False
        liftIO $ setString m "You lost !\nPress space to restart or\nescape to exit"
    when (xb + ballRadius > fromIntegral gameWidth) $ do
        putRunning False
        liftIO $ setString m "You lost !\nPress space to restart or\nescape to exit"
    when (yb - ballRadius < 0) $ do
        putBallAngle (-angle)
        liftIO $ setPosition b (Vector2D xb (ballRadius + 0.1))
    when (yb + ballRadius > fromIntegral gameHeight) $ do
        putBallAngle (-angle)
        liftIO $ setPosition b (Vector2D xb (fromIntegral gameHeight - ballRadius - 0.1))

    --Check the collisions between the ball and the paddles
    --Left paddle
    r <- liftIO randomIO
    when (xb - ballRadius < xl + xp / 2 && xb - ballRadius > xl && yb + ballRadius >= yl - yp / 2 && yb - ballRadius <= yl + yp / 2) $ do
        if (yb > yl)
            then putBallAngle (pi - angle + r * pi / 9)
            else putBallAngle (pi - angle - r * pi / 9)
        liftIO $ setPosition b (Vector2D (xl + ballRadius + xp / 2 + 0.1) yb)
    --Right paddle
    when (xb + ballRadius > xr - xp / 2 && xb + ballRadius < xr && yb + ballRadius >= yr - yp / 2 && yb - ballRadius <= yr + yp / 2) $ do
        if (yb > yr)
            then putBallAngle (pi - angle + r * pi / 9)
            else putBallAngle (pi - angle - r * pi  / 9)
        liftIO $ setPosition b (Vector2D (xr - ballRadius - xp / 2 - 0.1) yb)


runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main :: IO ()
main = do
    (env, st) <- initEnv
    runLoop env st

