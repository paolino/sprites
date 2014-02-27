
{-# LANGUAGE ViewPatterns, GADTs, FlexibleContexts  #-}

module GUI where
  
import Graphics.UI.Gtk.OpenGL 
import Graphics.UI.Gtk hiding (Point, Object)
import Graphics.Rendering.OpenGL 
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.List.Zipper
import Sprite.Widget (graphing) 
import Sprite.Logic
import GL

run  :: Eq (SocketName a) =>
     (Point -> a -> STM a)
     -> (ScrollDirection -> Point -> a -> STM a)
     -> (Object a -> IO ())
     -> TVar (Zipper (Graph a))
     -> IO ()

run setSynth scrollSynth renderSynth ref = do
  initGUI
  bootGL
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1
  connects <- graphing setSynth scrollSynth renderSynth ref 
  set window [containerChild := connects] 
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just
  mainGUI



