{-# LANGUAGE ViewPatterns, GADTs, FlexibleContexts, DataKinds  #-}

module Sprite.Widget where


import Control.Arrow ((***))

import Graphics.UI.Gtk hiding (Point, Socket, Object,focus)
import Graphics.UI.Gtk.OpenGL hiding (Sink)
import Graphics.Rendering.OpenGL hiding (Sink)

import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Trans

import Data.List.PointedList
	
import Sprite.GL (mkCanva)
import Sprite.Logic
import Sprite.D2
import Control.Lens (view, alongside)


cursor :: PointedList a -> a
cursor = view focus

toGLfloat :: Double -> GLfloat
toGLfloat = realToFrac


-- | projection aware rendering of an object
renderAGL :: (a -> IO ()) -> RenderObject IO a
renderAGL f (Affine (toGLfloat -> cx,toGLfloat -> cy) (toGLfloat -> sx,toGLfloat -> sy)) x = do
	preservingMatrix $ do
		translate (Vector3 cx cy 0)
		scale sx sy 1
		preservingMatrix $ do 
			translate (Vector3 (-0.5) (-0.5) (0 :: GLfloat))
			f x 

renderEdgeGL :: RenderEdge IO 
renderEdgeGL (Edge x y) = renderEdgeGL' (view point x) (view point y)

renderEdgeGL' :: Point -> Point -> IO ()
renderEdgeGL' (toGLfloat -> x,toGLfloat -> y) (toGLfloat -> x1,toGLfloat -> y1) = do 
   renderPrimitive Lines $ do
	vertex $ Vertex3 x1 y1 0 
	vertex $ Vertex3 x y 0 

addGraph ref x = modifyTVar ref $ insertRight  x 
graphing 
	:: (Point -> a -> STM a) -- react to a left click inside the widget 
	-> (ScrollDirection -> Point -> a  -> STM a)  -- react to a scrolling inside a widget
	-> (a  -> IO ())  -- GL renders an Object a
        -> RenderSocket IO Input
        -> RenderSocket IO Output
	->  TVar (PointedList (Graph a)) -- shared state of the graph, with undo and redo
	-> IO GLDrawingArea
graphing  innerclick innerscroll renderA renderSI renderSO ref = do
  connecting <- newTVarIO Nothing
  coo <- newTVarIO (0,0)
  size <- newTVarIO  (1,1)
  lineSmooth $= Enabled

  connects <- mkCanva size . const $  do
	  g <- fmap cursor . atomically $ readTVar ref
	  c <- atomically $ readTVar coo
	  conn <- atomically $ readTVar connecting
	  g' <- case conn of 
		Nothing -> return g
		Just f -> atomically (f c) 
	  renderGraph renderEdgeGL renderSI renderSO (renderAGL renderA) g' c 

  widgetSetEvents connects [AllEventsMask]
  on connects buttonPressEvent $ do
	b <- eventButton
	cl <- eventClick
	ms <- eventModifier
	liftIO . atomically $ do
		
		g  <- cursor `fmap` readTVar ref 
		c  <- readTVar coo
		case cl of
			TripleClick -> return ()
			DoubleClick -> return () 
			SingleClick -> 	case b of
				LeftButton -> case Control `elem` ms of
					True -> addGraph ref (deleteEdge c g)
					False -> case Shift `elem` ms of
						False -> writeTVar connecting . Just $ return . moveVertex c g
						True -> writeTVar connecting . Just $ \c -> sendToVertex innerclick c g
				RightButton -> writeTVar connecting $ fmap (fmap return) $ newEdge c g 
				MiddleButton -> writeTVar connecting $ fmap (fmap return) $ modifyEdge c g  
				_ -> return ()
		return True
  on connects scrollEvent $ do
	d <- eventScrollDirection
	ms <- eventModifier
	liftIO . atomically $ do 
		g <- cursor `fmap`  readTVar ref 
		c  <- readTVar coo
		let f = case d of 
			ScrollUp -> (* 1.1)
			ScrollDown -> (/1.1)
		case ms == [Control] of
			True -> addGraph ref $ scaleYVertex c g f
			False -> case ms == [Shift] of 
				True -> sendToVertex (innerscroll d) c g  >>= addGraph ref 
				False -> addGraph ref . flip (scaleXVertex c) f $ scaleYVertex c g f
			
	return True
  on connects keyPressEvent $ do
	v <- eventKeyVal
	ms <- eventModifier
	liftIO . atomically $ do
		c  <- readTVar coo
		g  <-  cursor `fmap` readTVar ref 
		case keyName v of
			"c" ->  case cloneVertex c g of
					Nothing -> return ()
					Just f ->  writeTVar connecting . Just $ return . f
			"d" -> addGraph ref $ deleteVertex c g
			
			"z" -> case ms of
					[Control] -> modifyTVar ref tryPrevious		
					_ -> return ()
			"y" -> case ms of
					[Control] -> modifyTVar ref tryNext
					_ -> return ()
			_ -> return ()
	return True
  on connects keyReleaseEvent $ do
	v <- eventKeyVal
	liftIO . atomically $ do
		c  <- readTVar coo
		g  <-  cursor `fmap` readTVar ref 
		f <- readTVar connecting 
		case keyName v of
			_ -> case f of 
				Just f -> do 
					f c >>= addGraph ref 
					writeTVar connecting Nothing
				Nothing -> return ()
	return True

  on connects buttonReleaseEvent $ do
	b <- eventButton
	liftIO . atomically $ do
		g <- cursor `fmap`  readTVar ref 
		c <- readTVar coo
		f <- readTVar connecting 
		case b of 
			_ -> case f of 
				Just f -> do 
					f c >>= addGraph ref 
					writeTVar connecting Nothing
				Nothing -> return ()

	return True
  on connects motionNotifyEvent $ do
        (x,y) <- eventCoordinates
	liftIO . atomically $ do
		(wg,hg) <- readTVar size
		writeTVar coo (realToFrac x/fromIntegral wg,realToFrac (fromIntegral hg - y)/fromIntegral hg)
	return True
  return connects			
			
