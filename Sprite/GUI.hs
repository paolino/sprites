
{-# LANGUAGE ViewPatterns, GADTs, FlexibleContexts, DataKinds  #-}

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
import Sprite.D2

run  :: (Point -> a -> STM a)
     -> (ScrollDirection -> Point -> a -> STM a)
     -> (a -> IO ())
     -> RenderSocket IO Input
     -> RenderSocket IO Output
     -> TVar (PointedList (Graph a))
     -> IO ()

run setx scrollx renderx renderSI renderSO ref = do
  initGUI
  bootGL
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1
  connects <- graphing setx scrollx renderx renderSI renderSO ref 
  set window [containerChild := connects] 
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just
  mainGUI



