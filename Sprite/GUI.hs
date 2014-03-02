
{-# LANGUAGE ViewPatterns, GADTs, FlexibleContexts  #-}

module Sprite.GUI where
  
import Graphics.UI.Gtk.OpenGL 
import Graphics.UI.Gtk hiding (Point, Object)
import Graphics.Rendering.OpenGL 
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.List.PointedList
import Sprite.Widget (graphing) 
import Sprite.Logic
import Sprite.GL

run  :: (Eq (SocketName a), Eq (ControlName a)) =>
     LensesOf a
     -> (Point -> a -> STM a)
     -> (ScrollDirection -> Point -> a -> STM a)
     -> (Object a -> IO ())
     -> TVar (PointedList (Graph a))
     -> IO ()

run le setx scrollx renderx ref = do
  initGUI
  bootGL
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1
  connects <- graphing le setx scrollx renderx ref 
  set window [containerChild := connects] 
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just
  mainGUI



