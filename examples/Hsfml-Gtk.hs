{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent.MVar
import Foreign
import Graphics.UI.Gtk hiding (Color)
import Graphics.UI.GtkInternals
import SFML.Graphics.Color
import SFML.Graphics.RenderWindow
import SFML.Window.Window (ContextSettings(..))


main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    area <- drawingAreaNew

    containerAdd window area
    widgetAddEvents area [AllEventsMask]
    widgetSetCanFocus area True
    widgetSetSizeRequest area 600 600

    widgetRealize area
    widgetSetDoubleBuffered area False

    areaId <- drawableGetID =<< widgetGetDrawWindow area
    sfmlWin <- createRenderWindowFromHandle (fromNativeWindowId areaId) context

    setFramerateLimit sfmlWin 60

    mv <- newMVar 0

    _ <- onDestroy window mainQuit

    _ <- onExpose area $ myDraw area sfmlWin mv
    _ <- onDestroy area $ myClose sfmlWin

    widgetShowAll window
    mainGUI

    where 
        context = ContextSettings 24 24 0 0 0


myDraw :: WidgetClass self => self -> RenderWindow -> MVar Word8 -> e -> IO Bool
myDraw area sfmlWin mv _event = do
    val <- modifyMVar mv (\x -> return (x+1, x))
    clear sfmlWin $ Color 0 192 val 255
    display sfmlWin
    widgetQueueDraw area
    return True

myClose :: RenderWindow -> IO ()
myClose sfmlWin = do
    close sfmlWin


widgetRealize :: WidgetClass self => self -> IO ()
widgetRealize self =
    (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_realize argPtr1) (toWidget self)

foreign import ccall "gtk_widget_realize"
    gtk_widget_realize :: ((Ptr Widget) -> (IO ()))

