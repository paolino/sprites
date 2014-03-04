{-# LANGUAGE TemplateHaskell, DeriveFunctor, ExistentialQuantification, GADTs, TypeFamilies, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, DeriveTraversable, DeriveFoldable, DataKinds, ScopedTypeVariables, Rank2Types, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes, NoMonomorphismRestriction #-}
module Sprite.Logic where

import Prelude hiding (sequence, any, foldr, elem, mapM, concat)
import Data.List hiding (any, foldr, elem, concat)
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Control.Arrow

import Control.Monad.Trans
import Control.Monad (liftM3, liftM2, liftM4, liftM, liftM5)
import Data.Typeable
import Control.Lens hiding (Affine)
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative

import Data.Functor.Identity
import Data.Binary

import Data.Monoid
import Debug.Trace
import Data.Graph.Topsort

import Data.Ord (comparing)
import Sprite.D2
-- | a type associated to index the socket type
type Net = String

-- | 2 kinds for a socket
data Versus = Input | Output 

newtype IControl = IControl Int deriving (Num,Ord,Eq, Enum, Show, Binary)

-- | sockets
data Socket :: Versus -> * where
	SInput 	:: Net   -- net name
                -> Point
		-> Socket Input 
	SOutput :: Net     -- socket name
                -> Point
		-> Socket Output 

point = lens f g where
        f :: Socket b -> Point
        f (SInput n p) = p
        f (SOutput n p) = p
        g :: Socket b -> Point -> Socket b
        g (SInput n p) q = SInput n q
        g (SOutput n p) q= SOutput n q

name = lens f g where
        f :: Socket b -> Net
        f (SInput n p) = n
        f (SOutput n p) = n
        g :: Socket b -> Net -> Socket b
        g (SInput _ p) n = SInput n p
        g (SOutput _ p) n = SOutput n p
instance  Binary (Socket Input) where
        put (SInput x y ) = put x >> put y
        get = liftM2 SInput get get 
instance Binary (Socket Output) where
        put (SOutput x y ) = put x >> put y
        get = liftM2 SOutput get get

-- transpose and scale
data Affine =  Affine 
	{	_affineTranspose :: Point -- ^ transposition point
	,	_affineScale :: Point  -- ^ two dim scaling factor
	}
instance  Binary Affine where
        put (Affine x y) = put x >> put y 
        get = liftM2 Affine get get 

$(makeLenses ''Affine)

-- counting sockets
newtype ISocket (a :: Versus) = ISocket Int deriving (Num,Ord,Eq, Enum,Binary)

-- an oject with its sockets
data Object a = Object 
	{ 	_objectInputs :: [Socket Input]
	,	_objectOutputs :: [Socket Output]
        ,       _object :: a
	}
$(makeLenses ''Object)

instance  Binary a => Binary (Object  a) where
        put (Object x y z) = put x >> put y  >> put z
        get = liftM3 Object get get get

-- lensing type
type ObjectLens a b = Lens' (Object a) [Socket b]

-- an edge of something
data Edge a  = Edge
	{ 	_edgeStart :: a Output
	, 	_edgeEnd :: a Input
	} 
	
$(makeLenses ''Edge)

instance (Binary (a Output), Binary (a Input)) => Binary (Edge a) where
        put (Edge x y) = put x >> put y 
        get = liftM2 Edge get get 


-- object counting
newtype IObject = IObject Int  deriving (Num,Ord,Eq, Enum, Show, Binary)

-- indexing a socket 
data ISocketObj a = ISocketObj 
	{	_isobject  :: IObject 
	,	_isocket   :: ISocket a
	} deriving (Eq,Ord)
$(makeLenses ''ISocketObj)
-- edge counting

instance Binary (ISocketObj a) where
        put (ISocketObj x y) = put x >> put y 
        get = liftM2 ISocketObj get get 


newtype IEdge = IEdge Int  deriving (Num,Ord,Eq, Enum,Binary)


-- | Database of objects and cords
data Graph a = Graph
	{	_vertexes :: M.Map IObject (Affine, Object a) -- objects with their geometrical status
	,	_edges :: M.Map IEdge (Edge ISocketObj) -- edges
	}

$(makeLenses ''Graph)

instance Binary a => Binary (Graph a) where
        put (Graph x y) = put x >> put y 
        get = liftM2 Graph get get  

-----------------------------
---- rendering --------------
-----------------------------
glass :: Point -> Affine -> Affine
glass p@(px,py) (Affine t@(tx,ty) s@(sx,sy)) = Affine (tx,ty) (sx / d , sy / d)
		where d = ( (+ 0.04) $ distance p t) ** 0.2

-- | render the graph vertex using the Object instance of Renderable and the edges using the given function. The point , representing the pointer is used to compute the distances from the objects which are passed along for rendering purpouses.


-- | render an edge action
type RenderEdge m  = Edge Socket ->  m ()

-- render an object action
type RenderObject m a = Affine -> a -> m () 

type RenderSocket m b = Socket b -> m () 

csplace :: Affine -> Point -> Point
csplace (Affine c k)  x =  c .+. ((x .-. (0.5,0.5)) .*. k)

soplace p a = point %~ csplace (glass p a) 

realizeSocket ::  Point -> ObjectLens a b -> Graph a -> ISocketObj b -> (Socket b)
realizeSocket p f g  (ISocketObj io (ISocket iso)) = maybe (error "realizeSocket index error") id $ do
        (a,x) <- g ^. vertexes . at io 
	let so = (x ^. f) !! iso
	return (soplace p a so)

realizeSocketOutputName :: Graph a -> ISocketObj Output -> Net
realizeSocketOutputName g i = realizeSocket undefined objectOutputs  g i ^. name

realizeSocketInputName :: Graph a -> ISocketObj Input -> Net
realizeSocketInputName g i = realizeSocket undefined objectInputs  g i ^. name


realizeEdge :: Graph a -> Edge ISocketObj -> Point -> Edge Socket 
realizeEdge g (Edge x y) p = Edge  (realizeSocket p objectOutputs g $ x) (realizeSocket p objectInputs g $ y)

 

renderGraph :: (Monad m, Functor m) => RenderEdge  m  -> RenderSocket m Input -> RenderSocket m Output -> RenderObject m a -> Graph a -> Point -> m ()
renderGraph re ris ros rob g@(Graph sps es) p = do
	forM_ es $ \e -> re $ realizeEdge g e p 
        forM_ sps $ \(a, x) -> do
                forM_ (x ^. objectInputs) (ris . soplace p a) 
                forM_ (x ^. objectOutputs) (ros . soplace p a) 
                rob (glass p a) (x ^. object)

affineBack :: Affine -> Point -> Point
affineBack (Affine c k) x = ((x .-. c) ./. k) .+. (0.5,0.5)

socketDistance :: Point -> Socket b -> Distance
socketDistance p s = distance p $ s ^. point


-- | Distance from the affine
affineDistance :: Affine -> Point -> Distance
affineDistance m c = distance c $ m ^. affineTranspose



-----------------------------------
--- Comonadic Graph Interface -----
-----------------------------------


-- vertex can be deleted or cloned. Deleting will imply the deletion of all edges attached. Cloning will produce an unconnected vertex.

--  all interfaces functions start from the pointer position. Some just produce a new Graph, others produces a Graph after a second point.
-- care is taken that the Graph is correct
-- sort all sockets by distance from a point

nearestSockets' 
     :: ObjectLens a b 
     -> Point
     -> Graph a
     -> [(Distance, ISocketObj b)]
nearestSockets' f c g = sortBy (comparing fst) $ do
			(io, (a,o)) <- M.assocs $ g ^. vertexes
			(iso ,c') <- zip [fromIntegral 0 ..] . map (csplace a . view point) $ o ^. f
			return  (distance c c', ISocketObj io iso)
nearestSockets 
     :: ObjectLens a b 
     -> Point
     -> Graph a
     -> [ISocketObj b]
nearestSockets f c g = map snd $ nearestSockets' f c g

nearestVertexes :: Point -> Graph a -> [IObject]
nearestVertexes p g = map snd . sortBy (comparing fst) $ 
           map (view (id `alongside` isobject)) (nearestSockets' objectInputs p g) 
        ++ map (view (id `alongside` isobject)) (nearestSockets' objectOutputs p g) 

-- sort edges by dimap (view (id `alongside` isobject)) (nearestSockets' objectOutputs p g)stance to a point, comparing the two distances from edge vertexes to the point sorted 


----- vertexes -------------

type Click m a = Point -> a -> m a
sendToVertex :: (Functor m, Applicative m) => Click m a -> Point -> Graph a  -> m (Graph a)
sendToVertex f p g = case nearestVertexes p g of 
	[] -> pure g
	io : _ -> let Just a = g ^? vertexes . ix io . _1 in 
                vertexes %%~ (ix io (_2 . object %%~ f (affineBack (glass p a) p))) $ g 


scaleXVertex :: Point -> Graph a -> (Double -> Double) -> Graph a
scaleXVertex p g f = case nearestVertexes p g of 
	[] -> g
	io : _ -> vertexes %~ (M.adjust (_1 . affineScale . _1 %~ f) io) $ g

scaleYVertex :: Point -> Graph a -> (Double -> Double) -> Graph a
scaleYVertex p g f = case nearestVertexes p g of 
	[] -> g
	io : _ -> vertexes %~ (M.adjust (_1 . affineScale . _2 %~ f) io) $ g


removeEdge ::  IEdge -> Graph a -> Graph a
removeEdge i = edges %~ M.delete i 



-- all edges coming and going from something
vertexEdges :: forall a . Graph a -> IObject -> [IEdge]
vertexEdges g io = let 
		f :: Edge ISocketObj -> Bool
		f (Edge x y) = x ^. isobject == io || y ^. isobject  == io  
	in M.keys . M.filter f $ g ^. edges


-- | delete the nearest by socket vertex 
deleteVertex :: Point -> Graph a -> Graph a
deleteVertex p g = case nearestVertexes p g of
	[] -> g
	io: _ -> (vertexes . at io .~ Nothing) . foldr removeEdge g $ vertexEdges g io
 



newKey :: (Enum a, Num a) => M.Map a b -> a
newKey g 
	| M.null g = 0
	| otherwise = succ . fst $ M.findMax $ g 

_moveVertex :: IObject -> Graph m -> Point -> Graph m
_moveVertex io g p' = vertexes %~ (M.adjust (_1 . affineTranspose .~ p') io) $ g

-- | move the nearest by socket vertex
moveVertex :: Point -> Graph m -> Point -> Graph m
moveVertex p g p' = case nearestVertexes p g of
	[] -> g
	io: _ -> _moveVertex io g p'


cloneVertex :: Point -> Graph m -> Maybe (Point -> Graph m)
cloneVertex p g  = case nearestVertexes p g of
	[] -> Nothing
	io : _ -> let 
		nio = newKey $ g ^. vertexes
		in Just $ _moveVertex nio (vertexes %~ at nio .~ (g ^. vertexes . at io) $ g)



------ edges ---------------

sortEdges c g = sortBy (comparing fst) $ do
			(ie,e) <- M.assocs $ g ^. edges
			let 	f (Edge s1 s2) = (head $ sort [distance c (s1 ^. point), distance c (s2 ^. point)], ie)	
			return  . f $ realizeEdge g e c 

-- | delete the nearest edge to the given point
deleteEdge :: Point -> Graph m -> Graph m
deleteEdge p g = case sortEdges p g of
		[] -> g
		io: _ -> removeEdge (snd io) g 
		

addEdge :: Edge ISocketObj -> Graph a -> Graph a
addEdge e@(Edge es ee) g = (edges . at (newKey $ g ^. edges) .~ Just e) $ g 


tsortg c g = let
	conn (Edge i j) = (i ^. isobject,j ^. isobject)
	in  tsort $ map conn (M.elems  $ g ^. edges)

completeEdgeInput :: ISocketObj Input -> Graph a -> Point  -> Graph a
completeEdgeInput j g p = case filter ((==) (realizeSocketInputName g j) . realizeSocketOutputName g ) $ nearestSockets objectOutputs p g of
			[] -> g
			i : _ -> 	let 	g' = addEdge (Edge i j) g in 
					case tsortg undefined g' of
						Nothing -> g
						_ ->  g' --
 
completeEdgeOutput :: ISocketObj Output -> Graph a -> Point -> Graph a
completeEdgeOutput j g p = case filter ((==) (realizeSocketOutputName g j) . realizeSocketInputName g ) $  nearestSockets objectInputs p g of
			[] -> g
			i : _ -> let 	g' = addEdge (Edge j i) g 
				in case tsortg undefined g' of
					Nothing -> g
					_ ->  g'

findBestSocket :: ObjectLens a b -> Point -> Graph a -> [(Distance, ISocketObj b)]
findBestSocket f p g = map (first $ socketDistance p) . map (realizeSocket p f g &&& id) $ nearestSockets f p g 

newEdge :: Point -> Graph a -> Maybe (Point -> Graph a)
newEdge p g = let
	mi = over (traverse . _2) (\i -> completeEdgeInput i g) $ findBestSocket objectInputs p g
	mo = over (traverse . _2) (\i -> completeEdgeOutput i g) $ findBestSocket objectOutputs p g 
	in case sortBy (comparing fst) $ mi ++ mo of
		[] -> Nothing
		(_,f):_ -> Just f


-- | move one vertex of an edge
modifyEdge :: Point -> Graph a -> Maybe (Point -> Graph a)
modifyEdge p g = case sortEdges p g of
		[] -> Nothing
		(_,i): _ -> let
                        Just (j@(Edge ji je)) = g ^. edges . at i
                        Edge si se = realizeEdge g j p
                        in case socketDistance p si > socketDistance p se of
                                True ->  Just $ completeEdgeOutput (ji) (removeEdge i g)
                                False ->  Just $ completeEdgeInput (je) (removeEdge i g)
