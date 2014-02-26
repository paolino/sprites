{-# LANGUAGE TemplateHaskell, DeriveFunctor, ExistentialQuantification, GADTs, TypeFamilies, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, DeriveTraversable, DeriveFoldable, DataKinds, ScopedTypeVariables, Rank2Types, FlexibleContexts  #-}
module Sprite.Logic where

import Prelude hiding (sequence, any, foldr, elem, mapM)
import Data.List hiding (any, foldr, elem)
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Control.Arrow

import Control.Monad.Trans
import Data.Typeable
import Control.Lens hiding (set, Affine)
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative

import Data.Functor.Identity


import Data.Monoid
import Debug.Trace
import Data.Graph.Topsort

type Distance = Double
type Point = (Double , Double)

-- | 2D Operations, vector sum, difference and biscaling
(.+.),(.-.),(.*.),(./.) :: Point -> Point -> Point

(x,y) .+. (x1,y1) = (x + x1,y + y1)
(x,y) .-. (x1,y1) = (x - x1,y - y1)
(x,y) .*. (k,h) = (x*k,y*h)
(x,y) ./. (k,h) = (x/k,y/h)

-- | 2D euclidean distance
distance :: Point -> Point -> Distance
distance (x,y) (x1,y1) = sqrt ((x - x1) ^ 2 + (y - y1) ^ 2)

-- | midpoint
mid :: Distance -> Point -> Point -> Point
mid k (x,y) (x1,y1) = ((x + k*x1) / (k + 1) ,(y + k * y1) / (k + 1))


type family SocketName a

data Versus = Input | Output

type family Signal a 

data Socket a b where
	SInput 	:: Point 	   -- center of the socket
		-> Point 	   -- center of the widget
		-> [SocketName a]  -- possible inputs
		-> Socket a Input 
	SOutput :: Point 
		-> Point 
		-> SocketName a    -- socket name
		-> ([Signal a] -> a -> Signal a, Signal a) -- (computation function from all inputs to this output)
		-> Socket a Output  


-- point lenses for a Socket

point :: forall f a b . Functor f => (Point -> f Point) -> Socket a b -> f (Socket a b)
point = lens f g where
	f :: Socket a b -> Point
	f (SInput p _ _) = p
	f (SOutput p _ _ _) = p
	g :: Socket a b -> Point -> Socket a b
	g (SInput p s xs) q = SInput q s xs
	g (SOutput p s x f) q = SOutput q s x f

center :: forall f a b . Functor f => (Point -> f Point) -> Socket a b -> f (Socket a b)
center = lens f g where
	f :: Socket a b -> Point
	f (SInput _ p _) = p
	f (SOutput _ p _ _) = p
	g :: Socket a b -> Point -> Socket a b
	g (SInput s p xs) q = SInput s q xs
	g (SOutput s p x f ) q = SOutput s q x f
	
-- transpose and scale
data Affine =  Affine 
	{	_affineTranspose :: Point -- ^ transposition point
	,	_affineScale :: Point  -- ^ two dim scaling factor
	}

$(makeLenses ''Affine)

-- counting sockets
newtype ISocket (a :: Versus) = ISocket Int deriving (Num,Ord,Eq, Enum)

-- an oject with its sockets
data Object a = Object 
	{ 	_objectInputs :: M.Map (ISocket Input)  (Socket a Input) 
	,	_objectOutputs :: M.Map (ISocket Output) (Socket a Output)
	,	_object :: a
	}
$(makeLenses ''Object)


-- lensing type
type ObjectLens a b = Lens' (Object a) (M.Map (ISocket b) (Socket a b))

-- an edge of something
data Edge a = Edge 
	{ 	_edgeStart :: a Output
	, 	_edgeEnd :: a Input
	} 
$(makeLenses ''Edge)

type EdgeLens a b = Lens' (Edge a) (a b)

-- object counting
newtype IObject = IObject Int  deriving (Num,Ord,Eq, Enum, Show)

-- indexing a socket 
data ISocketObj (a::Versus) = ISocketObj 
	{	_isobject  :: IObject 
	,	_isocket   :: ISocket a
	} deriving (Eq,Ord)
$(makeLenses ''ISocketObj)

-- edge counting
newtype IEdge = IEdge Int  deriving (Num,Ord,Eq, Enum)


-- | Database of objects and cords
data Graph a = Graph
	{	_vertexes :: M.Map IObject (Affine, Object a) -- objects with their geometrical status
	,	_edges :: M.Map IEdge (Edge ISocketObj) -- edges
	,	_assigned :: M.Map (ISocketObj Input) (SocketName a) -- cache of the input connected to ensure one name each input
	}

$(makeLenses ''Graph)




-----------------------------
---- rendering --------------
-----------------------------

-- | render the graph vertex using the Object instance of Renderable and the edges using the given function. The point , representing the pointer is used to compute the distances from the objects which are passed along for rendering purpouses.


-- | render an edge action
type RenderEdge  a m = Edge (Socket a) -> m ()

-- render an object action
type RenderObject a m = Object a -> Affine -> m () 

renderGraph :: (Monad m, Functor m) => RenderEdge  a m -> RenderObject a m -> Graph a -> m ()
renderGraph re ro g@(Graph sps es as) = do
	forM_ sps $ \(a, x) -> ro x a 
	forM_ (M.elems es) $ \e -> case realizeEdge g e of 
		Nothing -> error "lookup index failed in render graph"
		Just eas -> re eas 

realizeEdge :: Graph a -> Edge ISocketObj -> (Maybe  (Edge (Socket a)))
realizeEdge g e = do 
	s1 <- realizeSocket objectOutputs g $ e ^. edgeStart 
	s2 <- realizeSocket objectInputs g $ e ^. edgeEnd
	return $ Edge s1 s2
 
realizeSocket ::  ObjectLens a b -> Graph a -> ISocketObj b -> Maybe (Socket a b)
realizeSocket f g  (ISocketObj io iso) = do
	(a,x) <- g ^. vertexes . at io 
	so <- x ^. f . at iso 
	return (placeSocket a $ so)
 
csplace :: Affine -> Point -> Point
csplace (Affine c k)  x =  c .+. ((x .-. (0.5,0.5)) .*. k)

affineBack :: Affine -> Point -> Point
affineBack (Affine c k) x = ((x .-. c) ./. k) .+. (0.5,0.5)

socketDistance :: Point -> Socket a b -> Distance
socketDistance p s = distance p $ s ^. point


-- | Distance from the affine
affineDistance :: Affine -> Point -> Distance
affineDistance m c = distance c $ m ^. affineTranspose

placeSocket :: Affine -> Socket a b -> Socket a b
placeSocket a = (center %~ csplace a) . (point %~ csplace a)


------------------------------------------
------ Evaluation --------------------------
------------------------------------------
touchObject :: Monoid (Signal a) => (ISocket Input -> [Signal a]) -> Object a -> Object a
touchObject f (Object is os x) = let 
	e (SOutput p q li (g,ro)) = SOutput p q li (g,g ys x)
	ys = map mconcat (map f $ M.keys is)
	in Object is (e `fmap` os) x

getValue :: Graph a -> IObject -> ISocket Input -> [Signal a]
getValue  g o i = let 
	ls = map (view edgeStart) . filter ((==) (ISocketObj o i) . view edgeEnd) $ M.elems $ g ^. edges
	check io = case realizeSocket objectOutputs g io of
		Nothing -> error "getValue index error"
		Just o -> o 
	in  map ((\(SOutput p q li (g,ro)) -> ro) . check) $ ls

updateObject :: Monoid (Signal a) => IObject -> Graph a -> Graph a
updateObject o g = vertexes . at o %~ fmap (second $  touchObject (getValue g o)) $ g

	
mkAcyclic :: Graph a -> [(IObject,IObject)]
mkAcyclic g = map (\(Edge u d) -> (u ^. isobject, d ^. isobject)) . M.elems $ g ^. edges

-- !!!!   tsort and updateObject


-----------------------------------
--- Comonadic Graph Interface -----
-----------------------------------


-- vertex can be deleted or cloned. Deleting will imply the deletion of all edges attached. Cloning will produce an unconnected vertex.

--  all interfaces functions start from the pointer position. Some just produce a new Graph, others produces a Graph after a second point.
-- care is taken that the Graph is correct


----- vertexes -------------


sendToVertex :: (Functor m, Applicative m) => Point -> Graph a -> (Point -> a -> m a) -> m (Graph a)
sendToVertex p g f = case nearestVertexes p g of 
	[] -> pure g
	io : _ -> let Just a = g ^? vertexes . at io . traverse . _1 in
		vertexes %%~ (ix io (_2 . object %%~ f (affineBack a p))) $ g 


scaleXVertex :: Point -> Graph a -> (Double -> Double) -> Graph a
scaleXVertex p g f = case nearestVertexes p g of 
	[] -> g
	io : _ -> vertexes %~ (M.adjust (_1 . affineScale . _1 %~ f) io) $ g

scaleYVertex :: Point -> Graph a -> (Double -> Double) -> Graph a
scaleYVertex p g f = case nearestVertexes p g of 
	[] -> g
	io : _ -> vertexes %~ (M.adjust (_1 . affineScale . _2 %~ f) io) $ g


-- filter all edges 
filterEdges :: Graph a -> (Edge ISocketObj -> Bool) -> [IEdge]
filterEdges g f = M.keys . M.filter f $ g ^. edges



-- all edges coming and going from something 
vertexEdges :: Graph a -> IObject -> [IEdge]
vertexEdges g io = filterEdges g (\x -> x ^. edgeStart . isobject == io || x ^. edgeEnd . isobject  == io ) 


-- | delete the nearest by socket vertex 
deleteVertex :: Point -> Graph a -> Graph a
deleteVertex p g = case nearestVertexes p g of
	[] -> g
	io: _ -> (vertexes . at io .~ Nothing) . foldr removeEdge g $ vertexEdges g io
 
-- sort all sockets by distance from a point
nearestSockets 
     :: ObjectLens a b 
     -> Point
     -> Graph a
     -> [ISocketObj b]
nearestSockets f c g = map snd . sortBy (comparing fst) $ do
			(io, (a,o)) <- M.assocs $ g ^. vertexes
			(iso ,c') <- M.assocs . fmap (csplace a . view point) $ o ^. f
			return  (distance c c', ISocketObj io iso)


nearestVertexes p g = map (view isobject) (nearestSockets objectInputs p g) ++ map (view isobject) (nearestSockets objectInputs p g)

-- sort edges by distance to a point, comparing the two distances from edge vertexes to the point sorted 
nearestEdges 
     :: Point
     -> Graph a
     -> [IEdge]
nearestEdges c g = map snd . sortBy (comparing fst) . catMaybes $ do
			(ie,e) <- M.assocs $ g ^. edges
			let f (Edge s1 s2) = (sort [distance c (s1 ^. point), distance c (s2 ^. point)], ie)	
			return  . fmap f $ realizeEdge g e 

-- assigned aware edge romoval, unsafe in the edge index
removeEdge ::  IEdge -> Graph a -> Graph a
removeEdge i g = let 
	e = ((g ^. edges) M.! i) ^. edgeEnd
	g' = edges %~ M.delete i $ g
	ies = socketEdges edgeEnd g e 
	fas = case ies of 
		[] -> const Nothing
		_ -> id
	in assigned . at e %~ fas $ g'

-- list all edges coming or going from a socket
socketEdges :: EdgeLens ISocketObj b -> Graph a -> ISocketObj b -> [IEdge]
socketEdges f g isoo  = filterEdges g (\x -> x ^. f == isoo) 


-- | move the nearest by socket vertex
moveVertex :: Point -> Graph m -> Point -> Graph m
moveVertex p g p' = case nearestVertexes p g of
	[] -> g
	io: _ -> _moveVertex io g p'

_moveVertex :: IObject -> Graph m -> Point -> Graph m
_moveVertex io g p' = vertexes %~ (M.adjust (_1 . affineTranspose .~ p') io) $ g

cloneVertex :: Point -> Graph m -> Maybe (Point -> Graph m)
cloneVertex p g  = case nearestVertexes p g of
	[] -> Nothing
	io : _ -> let 
		nio = newKey $ g ^. vertexes
		in Just $ _moveVertex nio (vertexes %~ at nio .~ (g ^. vertexes . at io) $ g)



------ edges ---------------

-- | delete the nearest edge to the given point
deleteEdge :: Point -> Graph m -> Graph m
deleteEdge p g = case nearestEdges p g of
	[] -> g
	io: _ -> removeEdge io g 

newEdge :: Eq (SocketName a) => Point -> Graph a -> Maybe (Point -> Graph a)
newEdge p g = let
	mi = findBestSocket objectInputs p g
	mo = findBestSocket objectOutputs p g 
	in case mi of
		Just (vi,i) -> case mo of
			Just (vo,o) -> if vi < vo then Just $ completeEdgeInput i g 
					else  Just $ completeEdgeOutput o g 
			Nothing -> Just $ completeEdgeInput i g 
		Nothing ->  fmap (\(_,o) -> completeEdgeOutput o g) $ mo

		


completeEdgeInput :: Eq (SocketName a) => ISocketObj Input -> Graph a -> Point  -> Graph a
completeEdgeInput j g p = 
	case realizeSocket objectInputs g j of
		Nothing -> error "completeEdgeOutput index error"
		Just (SInput _ _ ns) -> case filter (judgeOutputSockets ns g j ) $  nearestSockets objectOutputs p g of
			[] -> g
			i : _ -> let 	g' = addEdge (Edge i j) g
					es = map (\(Edge x y) -> (x ^. isobject, y ^. isobject))  (M.elems  $ g' ^. edges)

				in case tsort es of
					Nothing -> g
					_ ->  g'


judgeOutputSockets :: Eq (SocketName a) => [SocketName a] -> Graph a -> ISocketObj Input -> ISocketObj Output -> Bool
judgeOutputSockets ns g j i = case realizeSocket objectOutputs g i of
		Nothing -> error "judgeOutputSockets index error"
		Just (SOutput _ _ n _) -> g ^. assigned . at j == Just n || (g ^. assigned . at j == Nothing && n `elem` ns)
 
completeEdgeOutput :: Eq (SocketName a) => ISocketObj Output -> Graph a -> Point -> Graph a
completeEdgeOutput j g p = 
	case realizeSocket objectOutputs g j of
		Nothing -> error "completeEdgeOutput index error"
		Just (SOutput _ _ n _) -> case filter (judgeInputSockets n g) $  nearestSockets objectInputs p g of
			[] -> g
			i : _ -> let 	g' = addEdge (Edge j i) g
					es = map (\(Edge x y) -> (x ^. isobject, y ^. isobject))  (M.elems  $ g' ^. edges)
				in case tsort es of
					Nothing -> g
					_ ->  g'

judgeInputSockets :: Eq (SocketName a) => SocketName a -> Graph a -> ISocketObj Input -> Bool
judgeInputSockets n g i = case realizeSocket objectInputs g i of
		Nothing -> error "judgeInputSockets index error"
		Just (SInput _ _ ns) ->  g ^. assigned . at i == Just n || (g ^. assigned . at i == Nothing && n `elem` ns) 

newKey :: (Enum a, Num a) => M.Map a b -> a
newKey g 
	| M.null g = 0
	| otherwise = succ . fst $ M.findMax $ g 

addEdge :: Edge ISocketObj  -> Graph a -> Graph a
addEdge e g = (assigned . at (e ^. edgeEnd) .~ (Just . sone $ e ^. edgeStart)) . (edges . at (newKey $ g ^. edges) .~ Just e) $ g where
	sone s = case fmap (\(SOutput _ _ n _) -> n) $ realizeSocket objectOutputs g s of
		Nothing -> error "addEdge index error"
		Just x -> x

findBestSocket :: ObjectLens a b -> Point -> Graph a -> Maybe (Distance, ISocketObj b)
findBestSocket f p g = fmap (first $ socketDistance p) . listToMaybe . catMaybes . map (\(m,i) -> flip (,) i `fmap` m) 
	                     . map (realizeSocket f g &&& id) $ nearestSockets f p g 

-- | move one vertex of an edge
modifyEdge :: forall a . Eq (SocketName a) => Point -> Graph a -> Maybe (Point -> Graph a)
modifyEdge p g = case nearestEdges p g of
	[] -> Nothing
	i : _ -> case g ^. edges . at i of
		Nothing ->  error "modifyEdge index error" 
		Just j -> case realizeEdge g j of
			Nothing -> error "modifyEdge index error"
			Just (Edge si se)  -> case socketDistance p si > socketDistance p se of
				True -> Just $ completeEdgeOutput (j ^. edgeStart) (removeEdge i g)
				False -> Just $ completeEdgeInput (j ^. edgeEnd) (removeEdge i g)
	

