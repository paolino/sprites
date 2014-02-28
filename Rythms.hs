
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DataKinds, ViewPatterns,GADTs #-}


-- module Rythms where


import Control.Concurrent.STM
import Control.Concurrent

import Graphics.UI.Gtk hiding (Point,Signal, Object,get)
import Graphics.UI.Gtk.OpenGL  hiding (get)
import Graphics.Rendering.OpenGL  hiding (get,Projection)
import qualified Data.Map as M
import Control.Monad
import Sprite.Logic
import Sprite.Widget
import Control.Lens hiding (set)
-- import Haskell
import Data.Monoid
import Data.List.PointedList 

import Sprite.GUI
import Data.Binary

import Sprite.OpenGlDigits

data Synth = Pattern  | Synth | KnobI Int  deriving Show

instance Binary Synth where
        put (Pattern) = put 'a'
        put (Synth) = put 'b'
        put (KnobI x) = put 'c' >> put x
        get = do
                x <- get
                case x of 
                        'a' -> return Pattern
                        'b' -> return Synth 
                        'c' -> KnobI `fmap` get

type instance SocketName Synth = String 
data Control = Intero | Decimale

type instance ControlName Synth = Control


basePattern  = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		(M.singleton 0 (SOutput (1.1,0.5) (0.5,0.5) "pattern" ))
		(M.fromList $ zip [0..] [SControl (x,0.5) (0.5,0.5) Intero | x <- [0.1,0.2..0.9]])
		(Pattern )

baseSynth = Object
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		M.empty 
		(M.fromList $ zip [0..] [SControl (x,0.5) (0.5,0.5) Intero | x <- [0.5]])		
		(Synth)
baseKnobI n = Object
		M.empty
		M.empty 
		(M.fromList $ zip [0..] [SControl (x,0.5) (0.5,0.5) Intero | x <- [0.5]])		
		(KnobI n)

		
scrollSynth :: ScrollDirection -> Point -> Synth -> STM Synth
scrollSynth s (x,_) e@(KnobI y) 
	| x >=0 && x < 1 = return $ KnobI $ f y
	| otherwise = return e
	where f x = case s of
		ScrollUp ->  min 100 $ x + 1
		ScrollDown -> max 0 $ x - 1
		_ -> x
scrollSynth s (_,_) x = return x


setSynth :: Point -> Synth -> STM Synth
{-
setSynth (x,y) e@(KnobI q) 
	| x >=0 && x < 1 && y >= 0 && y < 1 = return . KnobI $ at m  .~ Just (realToFrac y) $ q
	| otherwise = return e
-}
setSynth _ x = return x



rbe (SInput (realToFrac -> x,realToFrac -> y) c _) = renderPrimitive LineLoop $ do
		vertex (Vertex2 (x - 0.05) (y - 0.05) :: Vertex2 GLfloat)
		vertex (Vertex2 (x + 0.05) (y - 0.05) :: Vertex2 GLfloat)
		vertex (Vertex2 (x + 0.05) (y + 0.05) :: Vertex2 GLfloat)
		vertex (Vertex2 (x - 0.05) (y + 0.05) :: Vertex2 GLfloat)
rbe (SOutput (realToFrac -> x,realToFrac -> y) c _ ) = do 
		renderPrimitive LineLoop $ do
			vertex (Vertex2 (x - 0.05) (y - 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x + 0.05) (y - 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x + 0.05) (y + 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x - 0.05) (y + 0.05) :: Vertex2 GLfloat)

rbe (SControl (realToFrac -> x,realToFrac -> y) c _ ) = do 
		renderPrimitive LineLoop $ do
			vertex (Vertex2 (x - 0.05) (y - 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x + 0.05) (y - 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x + 0.05) (y + 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x - 0.05) (y + 0.05) :: Vertex2 GLfloat)

renderSynth :: Object Synth -> IO ()
renderSynth (Object is os cs (Pattern ))  = do
			color (Color4 0.8 0.9 1 0.1:: Color4 GLfloat)
			forM_ [(0.1,0.1), (0.9,0.1),(0.9,0.9),(0.1,0.9)] $ \(xc,yc) -> 
				renderPrimitive Polygon $ forM_ [0,0.1.. 2*pi] $ \a -> do
					let 	x = xc + 0.1*cos a
					 	y = yc + 0.1*sin a
					vertex (Vertex2 x y:: Vertex2 GLfloat)

			color (Color4 0.8 0.9 1 0.1:: Color4 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)

			forM_ (M.elems is) rbe
			forM_ (M.elems os) rbe
			forM_ (M.elems cs) rbe

				
renderSynth (Object is os cs (Synth))  = do
			polygonSmooth $= Enabled
			lineSmooth $= Enabled
			color (Color4 0.4 0.9 0.9 0.1:: Color4 GLfloat)
			forM_ [(0.1,0.1), (0.9,0.1),(0.9,0.9),(0.1,0.9)] $ \(xc,yc) -> 
				renderPrimitive Polygon $ forM_ [0,0.1.. 2*pi] $ \a -> do
					let 	x = xc + 0.1*cos a
					 	y = yc + 0.1*sin a
					vertex (Vertex2 x y:: Vertex2 GLfloat)

			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)
			forM_ (M.elems is) rbe
			forM_ (M.elems os) rbe

			forM_ (M.elems cs) rbe
renderSynth (Object is os cs (KnobI v'))  = do
			polygonSmooth $= Enabled
			let 	x1 = 1/4
				x2 = 3/4
				v = fromIntegral v' / 100 * 0.7
			color (Color4 0.3 0.4 0.5 0.3:: Color4 GLfloat)
			renderNumberPosWH 0.5 0.5 (x2) (0.8) (1/8) (0.1) $  v'
				
			color (Color4 0.6 0.7 0.8 0.1:: Color4 GLfloat)
			renderPrimitive Quads $ do
				-- mcolor p 2 3 4 1
				vertex (Vertex2 x1 0.05 :: Vertex2 GLfloat)
				vertex (Vertex2 x2  0.05 :: Vertex2 GLfloat)
				-- mcolor p 4 3 2 1
				vertex (Vertex2 x2 (0.05 + realToFrac v) :: Vertex2 GLfloat)
				vertex (Vertex2 x1 (0.05 + realToFrac v) :: Vertex2 GLfloat)
			lineSmooth $= Enabled
			color (Color4 0.6 0.7 0.8 0.1:: Color4 GLfloat)
			renderNumberPosWH 0.5 0.5 (0.9) (0.5) (1/20) (1/10) $ v'
			-- renderNumberPosWH 0.5 0.5 0.1 0.5 (1/20) (1/10) $ fromIntegral (v M.! 0)
			color (Color4 0.4 0.9 0.9 0.1:: Color4 GLfloat)
			forM_ [(0.1,0.1), (0.9,0.1),(0.9,0.9),(0.1,0.9)] $ \(xc,yc) -> 
				renderPrimitive Polygon $ forM_ [0,0.1.. 2*pi] $ \a -> do
					let 	x = xc + 0.1*cos a
					 	y = yc + 0.1*sin a
					vertex (Vertex2 x y:: Vertex2 GLfloat)

			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)
			forM_ (M.elems is) rbe
			forM_ (M.elems cs) rbe


graph :: Graph Synth
graph = Graph (M.fromList $ 
	[ (0,(Affine (0.5,0.5) (0.1,0.06),basePattern))
	, (1,(Affine (0.5,0.5) (0.06,0.1),baseSynth))
	, (2,(Affine (0.5,0.5) (0.06,0.1),baseKnobI 50))
	]) M.empty M.empty

main = do
        -- g <- decodeFile "prova.rythms" 
        -- ref <- newTVarIO g
	ref <- newTVarIO (singleton graph)
	run setSynth scrollSynth renderSynth ref
        atomically (readTVar ref) >>= encodeFile "prova.rythms"
