{-# LANGUAGE TemplateHaskell, DeriveFunctor, ExistentialQuantification, GADTs, TypeFamilies, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, DeriveTraversable, DeriveFoldable, DataKinds, ScopedTypeVariables, Rank2Types, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Sprite.Logic where

import Prelude hiding (sequence, any, foldr, elem, mapM)
import Data.List hiding (any, foldr, elem)
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Control.Arrow

import Control.Monad.Trans
import Control.Monad (liftM3, liftM2, liftM4, liftM)
import Data.Typeable
import Control.Lens hiding (set, Affine)
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

-- | a type associated to index the socket type
type family SocketName a
type family ControlName a

-- | 2 kinds for a socket
data Versus = Input | Output | Duplex

-- | sockets
data Socket (a :: *) :: Versus -> * where
	SInput 	:: Point 	   -- center of the socket
		-> Point 	   -- center of the widget
		-> [SocketName a]  -- possible inputs
		-> Socket a Input 
	SOutput :: Point 
		-> Point 
		-> SocketName a    -- socket name
		-> Socket a Output 
	SControl :: Point
		-> Point
		-> ControlName a
		-> Socket a Duplex

instance (Binary (SocketName a)) => Binary (Socket a Input) where
        put (SInput x y z) = put x >> put y >> put z
        get = liftM3 SInput get get get
instance (Binary (SocketName a)) => Binary (Socket a Output) where
        put (SOutput x y z) = put x >> put y >> put z
        get = liftM3 SOutput get get get
instance (Binary (ControlName a)) => Binary (Socket a Duplex) where
        put (SControl x y z) = put x >> put y >> put z
        get = liftM3 SControl get get get

-- | point lenses for a Socket
point :: forall f a b . Functor f => (Point -> f Point) -> Socket a b -> f (Socket a b)
point = lens f g where
	f :: Socket a b -> Point
	f (SInput p _ _) = p
	f (SOutput p _ _ ) = p
	f (SControl p _ _ ) = p
	g :: Socket a b -> Point -> Socket a b
	g (SInput p s xs) q = SInput q s xs
	g (SOutput p s x ) q = SOutput q s x 
	g (SControl p s x ) q = SControl q s x 

-- | point lenses for a Socket
center :: forall f a b . Functor f => (Point -> f Point) -> Socket a b -> f (Socket a b)
center = lens f g where
	f :: Socket a b -> Point
	f (SInput _ p _) = p
	f (SOutput _ p _ ) = p
	f (SControl _ p _ ) = p
	g :: Socket a b -> Point -> Socket a b
	g (SInput s p xs) q = SInput s q xs
	g (SOutput s p x ) q = SOutput s q x 
	g (SControl s p x ) q = SControl s q x 
	
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
	{ 	_objectInputs :: M.Map (ISocket Input)  (Socket a Input) 
	,	_objectOutputs :: M.Map (ISocket Output) (Socket a Output)
	,	_objectControls :: M.Map (ISocket Duplex) (Socket a Duplex)
	,	_object :: a
	}
$(makeLenses ''Object)

instance (Binary (SocketName a), Binary (ControlName a), Binary a) => Binary (Object a) where
        put (Object x y z s) = put x >> put y >> put z >> put s
        get = liftM4 Object get get get get

-- lensing type
type ObjectLens a b = Lens' (Object a) (M.Map (ISocket b) (Socket a b))

-- an edge of something
data Edge a = Dir
	{ 	_edgeStart :: a Output
	, 	_edgeEnd :: a Input
	} 
	|
	Bi {
		_edgeEnds :: (a Duplex, a Duplex)
		}
	
$(makeLenses ''Edge)

instance (Binary (a Output), Binary (a Input), Binary (a Duplex)) =>  Binary (Edge a) where
        put (Dir x y) = put 'a' >> put x >> put y 
        put (Bi x) = put 'b' >> put x 
        get = do
		l <- get
		case l of 
			'a' -> liftM2 Dir get get 
			'b' -> liftM Bi get 

type EdgeLens a b = Lens' (Edge a) (a b)

-- object counting
newtype IObject = IObject Int  deriving (Num,Ord,Eq, Enum, Show, Binary)

-- indexing a socket 
data ISocketObj (a::Versus) = ISocketObj 
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
	,	_assigned :: M.Map (ISocketObj Input) (SocketName a) -- cache of the input connected to ensure one name each input
	}

$(makeLenses ''Graph)

instance (Binary (SocketName a), Binary (ControlName a), Binary a) => Binary (Graph a) where
        put (Graph x y z) = put x >> put y >> put z
        get = liftM3 Graph get get get


-----------------------------
---- rendering --------------
-----------------------------

-- | render the graph vertex using the Object instance of Renderable and the edges using the given function. The point , representing the pointer is used to compute the distances from the objects which are passed along for rendering purpouses.


-- | render an edge action
type RenderEdge  a m = Edge (Socket a) -> m ()

-- render an object action
type RenderObject a m = Object a -> Affine -> m () 

renderGraph :: (Monad m, Functor m) => RenderEdge  a m -> RenderObject a m -> Graph a -> Point -> m ()
renderGraph re ro g@(Graph sps es as) p = do
	forM_ sps $ \(a, x) -> ro x (glass p a) 
	forM_ (M.elems es) $ \e -> case realizeEdge g e p of 
		Nothing -> error "lookup index failed in render graph"
		Just eas -> re eas 

realizeEdge :: Graph a -> Edge ISocketObj -> Point -> (Maybe  (Edge (Socket a)))
realizeEdge g e@(Dir x y) p = do 
	s1 <- realizeSocket p objectOutputs g $ x
	s2 <- realizeSocket p objectInputs g $ y
	return $ Dir s1 s2
realizeEdge g e@(Bi (x,y)) p = do 
	s1 <- realizeSocket p objectControls g $ x
	s2 <- realizeSocket p objectControls g $ y
	return $ Bi (s1,s2)
 
realizeSocket ::  Point -> ObjectLens a b -> Graph a -> ISocketObj b -> Maybe (Socket a b)
realizeSocket p f g  (ISocketObj io iso) = do
	(a,x) <- g ^. vertexes . at io 
	so <- x ^. f . at iso 
	return (placeSocket (glass p a) $ so)
 
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

glass :: Point -> Affine -> Affine
glass p@(px,py) (Affine t@(tx,ty) s@(sx,sy)) = Affine (tx,ty) (sx / d , sy / d)
		where d = ( (+ 0.04) $ distance p t) ** 0.2

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
			(iso ,c') <- M.assocs . fmap (csplace a . view point) $ o ^. f
			return  (distance c c', ISocketObj io iso)
nearestSockets 
     :: ObjectLens a b 
     -> Point
     -> Graph a
     -> [ISocketObj b]
nearestSockets f c g = map snd $ nearestSockets' f c g

nearestVertexes p g = map snd . sortBy (comparing fst) $ 
        map (view (id `alongside` isobject)) (nearestSockets' objectInputs p g) ++ map (view (id `alongside` isobject)) (nearestSockets' objectOutputs p g) ++ map (view (id `alongside` isobject)) (nearestSockets' objectControls p g)

-- sort edges by dimap (view (id `alongside` isobject)) (nearestSockets' objectOutputs p g)stance to a point, comparing the two distances from edge vertexes to the point sorted 
nearestEdges 
     :: Point
     -> Graph a
     -> [IEdge]
nearestEdges c g = map snd . sortBy (comparing fst) . catMaybes $ do
			(ie,e) <- M.assocs $ g ^. edges
			let 	f (Dir s1 s2) = (sort [distance c (s1 ^. point), distance c (s2 ^. point)], ie)	
			 	f (Bi (s1,s2)) = (sort [distance c (s1 ^. point), distance c (s2 ^. point)], ie)	
			return  . fmap f $ realizeEdge g e c 


----- vertexes -------------


sendToVertex :: (Functor m, Applicative m) => Point -> Graph a -> (Point -> a -> m a) -> m (Graph a)
sendToVertex p g f = case nearestVertexes p g of 
	[] -> pure g
	io : _ -> let Just a = g ^? vertexes . at io . traverse . _1 in
		vertexes %%~ (ix io (_2 . object %%~ f (affineBack (glass p a) p))) $ g 


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
vertexEdges g io = filterEdges g f where
	f (Dir x y) = x ^. isobject == io || y ^. isobject  == io  
	f (Bi (x,y)) = x ^. isobject == io || y ^. isobject  == io  


-- | delete the nearest by socket vertex 
deleteVertex :: Point -> Graph a -> Graph a
deleteVertex p g = case nearestVertexes p g of
	[] -> g
	io: _ -> (vertexes . at io .~ Nothing) . foldr removeEdge g $ vertexEdges g io
 

-- assigned aware edge romoval, unsafe in the edge index
removeEdge ::  IEdge -> Graph a -> Graph a
removeEdge i g = case ((g ^. edges) M.! i) of
	Dir x e -> let 
		g' = edges %~ M.delete i $ g
		ies = socketEdges g e 
		fas = case ies of 
			[] -> const Nothing
			_ -> id
		in assigned . at e %~ fas $ g'
	Bi (x,y) -> edges %~ M.delete i $ g

-- list all edges coming or going from a socket
socketEdges :: Graph a -> ISocketObj Input -> [IEdge]
socketEdges g i  = map fst $ filter f (M.assocs $ g ^. edges) where
	f (_,Dir x y) = y == i
	f _ =  False

newKey :: (Enum a, Num a) => M.Map a b -> a
newKey g 
	| M.null g = 0
	| otherwise = succ . fst $ M.findMax $ g 
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

		
addEdge :: Edge ISocketObj  -> Graph a -> Point -> Graph a
addEdge e@(Dir es ee) g p = (assigned . at (ee) .~ (Just . sone $ es)) . (edges . at (newKey $ g ^. edges) .~ Just e) $ g where
	sone s = case fmap (\(SOutput _ _ n ) -> n) $ realizeSocket p objectOutputs g s of
		Nothing -> error "addEdge index error"
		Just x -> x

addEdge e@(Bi (es,ee)) g p = (edges . at (newKey $ g ^. edges) .~ Just e) $ g where

judgeOutputSockets :: Eq (SocketName a) => Point -> [SocketName a] -> Graph a -> ISocketObj Input -> ISocketObj Output -> Bool
judgeOutputSockets p ns g j i = case realizeSocket p objectOutputs g i of
		Nothing -> error "judgeOutputSockets index error"
		Just (SOutput _ _ n) -> g ^. assigned . at j == Just n || (g ^. assigned . at j == Nothing && n `elem` ns)

judgeInputSockets :: Eq (SocketName a) => Point -> SocketName a -> Graph a -> ISocketObj Input -> Bool
judgeInputSockets p n g i = case realizeSocket p objectInputs g i of
		Nothing -> error "judgeInputSockets index error"
		Just (SInput _ _ ns) ->  g ^. assigned . at i == Just n || (g ^. assigned . at i == Nothing && n `elem` ns) 

judgeDuplexSockets :: Eq (ControlName a) => Point -> ControlName a -> Graph a -> ISocketObj Duplex -> Bool
judgeDuplexSockets p n g i = case realizeSocket p objectControls g i of
		Nothing -> error "judgeDuplexSockets index error"
		Just (SControl _ _ n') ->  n' == n
tsortg g = let
	conn (Dir i j) = (i ^. isobject,j ^. isobject)
	conn (Bi (i,j)) = (i ^. isobject ,j ^. isobject)
	in  tsort $ map conn  (M.elems  $ g ^. edges)
completeEdgeInput :: Eq (SocketName a) => ISocketObj Input -> Graph a -> Point  -> Graph a
completeEdgeInput j g p = 
	case realizeSocket p objectInputs g j of
		Nothing -> error "completeEdgeInput index error"
		Just (SInput _ _ ns) -> case filter (judgeOutputSockets p ns g j) $  nearestSockets objectOutputs p g of
			[] -> g
			i : _ -> 	let 	g' = addEdge (Dir i j) g p in 
					case tsortg g' of
						Nothing -> g
						_ ->  g'


 
completeEdgeOutput :: Eq (SocketName a) => ISocketObj Output -> Graph a -> Point -> Graph a
completeEdgeOutput j g p = 
	case realizeSocket p objectOutputs g j of
		Nothing -> error "completeEdgeOutput index error"
		Just (SOutput _ _ n ) -> case filter (judgeInputSockets p n g) $  nearestSockets objectInputs p g of
			[] -> g
			i : _ -> let 	g' = addEdge (Dir j i) g p
				in case tsortg g' of
					Nothing -> g
					_ ->  g'

completeEdgeDuplex :: Eq (ControlName a) => ISocketObj Duplex -> Graph a -> Point -> Graph a
completeEdgeDuplex j g p = 
	case realizeSocket p objectControls g j of
		Nothing -> error "completeEdgeDuplex index error"
		Just (SControl _ _ n ) -> case filter (judgeDuplexSockets p n g) $  nearestSockets objectControls p g of
			[] -> g
			i : _ -> let 	g' = addEdge (Bi (j,i)) g p
				in case tsortg g' of
					Nothing -> g
					_ ->  g'
findBestSocket :: ObjectLens a b -> Point -> Graph a -> Maybe (Distance, ISocketObj b)
findBestSocket f p g = fmap (first $ socketDistance p) . listToMaybe . catMaybes . map (\(m,i) -> flip (,) i `fmap` m) 
	                     . map (realizeSocket p f g &&& id) $ nearestSockets f p g 

newEdge :: (Eq (SocketName a), Eq (ControlName a)) => Point -> Graph a -> Maybe (Point -> Graph a)
newEdge p g = let
	mi = over (traverse . _2) (\i -> completeEdgeInput i g) $ findBestSocket objectInputs p g
	mo = over (traverse . _2) (\i -> completeEdgeOutput i g) $ findBestSocket objectOutputs p g 
	mc = over (traverse . _2) (\i -> completeEdgeDuplex i g) $ findBestSocket objectControls p g 
	in case sortBy (comparing fst) $ catMaybes [mi,mo,mc] of
		[] -> Nothing
		(_,f):_ -> Just f



-- | move one vertex of an edge
modifyEdge :: forall a . (Eq (SocketName a), Eq (ControlName a)) => Point -> Graph a -> Maybe (Point -> Graph a)
modifyEdge p g = case nearestEdges p g of
	[] -> Nothing
	i : _ -> case g ^. edges . at i of
		Nothing ->  error "modifyEdge index error" 
		Just j@(Dir ji je) -> case realizeEdge g j p of
			Just (Dir si se)  -> case socketDistance p si > socketDistance p se of
				True -> Just $ completeEdgeOutput (ji) (removeEdge i g)
				False -> Just $ completeEdgeInput (je) (removeEdge i g)
			_ -> error "modifyEdge index error"
		Just j@(Bi (ji,je)) -> case realizeEdge g j p of
			Just (Bi (si,se))  -> case socketDistance p si > socketDistance p se of
				True ->  Just $ completeEdgeDuplex (ji) (removeEdge i g)
				False ->  Just $ completeEdgeDuplex (je) (removeEdge i g)
			_ -> error "modifyEdge index error"

--------------------------

{-
mkAcyclic g =  map (\(Edge x y) -> (y ^. isobject, x ^. isobject))  (M.elems  $ g ^. edges)
mkObjects g = fmap  (view $ _2 . object) (g ^. vertexes) 	
-}
