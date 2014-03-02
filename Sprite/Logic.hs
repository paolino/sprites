{-# LANGUAGE TemplateHaskell, DeriveFunctor, ExistentialQuantification, GADTs, TypeFamilies, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, DeriveTraversable, DeriveFoldable, DataKinds, ScopedTypeVariables, Rank2Types, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes, NoMonomorphismRestriction #-}
module Sprite.Logic where

import Prelude hiding (sequence, any, foldr, elem, mapM)
import Data.List hiding (any, foldr, elem)
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


newtype IControl = IControl Int deriving (Num,Ord,Eq, Enum, Show, Binary)
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
        put (SControl x y z) = put x >> put y  >> put z
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
	f (SInput _ p _ ) = p
	f (SOutput _ p _ ) = p
	f (SControl _ p _) = p
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

data Edges = Dep | Jumper

type family Start (a :: Edges) :: Versus
type family End (a :: Edges) :: Versus

type instance Start Dep = Output
type instance End Dep = Input
type instance End Jumper = Duplex
type instance Start Jumper = Duplex

-- an edge of something
data Edge a (b :: Edges) = Edge
	{ 	_edgeStart :: a (Start b)
	, 	_edgeEnd :: a (End b)
	} 
{-
	|
	Bi {
		_edgeEnds :: (a Duplex, a Duplex)
		}
-}
	
$(makeLenses ''Edge)
instance (Binary (a (Start b)), Binary (a (End b)) ) =>  Binary (Edge a b) where
        put (Edge x y) = put x >> put y 
        get = liftM2 Edge get get 

type EdgeLens a b c = Lens' (Edge a c) (a b)

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


newtype IEdge (b::Edges) = IEdge Int  deriving (Num,Ord,Eq, Enum,Binary)


-- | Database of objects and cords
data Graph a = Graph
	{	_vertexes :: M.Map IObject (Affine, Object a) -- objects with their geometrical status
	,	_edgesDep :: M.Map (IEdge Dep) (Edge ISocketObj Dep) -- edges
	,	_edgesJumper :: M.Map (IEdge Jumper) (Edge ISocketObj Jumper) -- edges
	,	_assigned :: M.Map (ISocketObj Input) (SocketName a) -- cache of the input connected to ensure one name each input
	, 	_gisles :: [[ISocketObj Duplex]]
	}

$(makeLenses ''Graph)

instance (Binary (SocketName a), Binary (ControlName a), Binary a) => Binary (Graph a) where
        put (Graph x y z d c ) = put x >> put y >> put z >> put d >> put c
        get = liftM5 Graph get get get get get

-------------------------------
------ updating ------------
----------------------------

lensAt :: ISocketObj Duplex -> Graph a -> (a,ISocket Duplex) 
lensAt  x g = let 
	y = snd $ (g ^. vertexes) M.! (x ^. isobject)
	in (view object y,x ^. isocket)

type family ControlValue a 
type LensesOf a = a -> ISocket Duplex -> Lens' a (ControlValue a) 

electrical' 
     :: forall a . LensesOf a
     -> ISocketObj 'Duplex
     -> [ISocketObj 'Duplex]
     -> Graph a
     -> Graph a

electrical' f x xs g = let
	v = let (y,i) = lensAt x g in y ^. f y i 
	fg :: ISocketObj Duplex -> Graph a -> Graph a
	fg x g = let 
		(y,i) = lensAt x g  
		in vertexes %~ M.adjust (second $ object . f y i .~ v) (x ^. isobject) $ g
	in foldr fg g xs

electrical :: forall a . LensesOf a
     -> ISocketObj 'Duplex
     -> Graph a
     -> Graph a
electrical f x g = let
	is = filter (x `elem`) $ g ^. gisles
	in case is of 
		[] -> g
		[xs] -> electrical' f x (delete x xs) g 
electricalO :: forall a . LensesOf a
     -> IObject
     -> Graph a
     -> Graph a
electricalO f i g = let
	-- select touched groups
	js :: [([ISocketObj Duplex],[ISocketObj Duplex])]
	js = map (partition ((== i) . view isobject)) $ g ^. gisles
	is = map (head *** id) . filter (not . null . fst) $ js  
	in foldr  (uncurry $ electrical' f) g is 


updateIsles :: Graph a -> Graph a
updateIsles g = gisles .~ isles (map (\(Edge x y) -> (x,y)) . M.elems $ g ^. edgesJumper) $ g

-----------------------------
---- rendering --------------
-----------------------------
glass :: Point -> Affine -> Affine
glass p@(px,py) (Affine t@(tx,ty) s@(sx,sy)) = Affine (tx,ty) (sx / d , sy / d)
		where d = ( (+ 0.04) $ distance p t) ** 0.2

-- | render the graph vertex using the Object instance of Renderable and the edges using the given function. The point , representing the pointer is used to compute the distances from the objects which are passed along for rendering purpouses.


-- | render an edge action
type RenderEdge a m b = Edge (Socket a) b -> m ()

-- render an object action
type RenderObject a m = Object a -> Affine -> m () 

csplace :: Affine -> Point -> Point
csplace (Affine c k)  x =  c .+. ((x .-. (0.5,0.5)) .*. k)

placeSocket :: Affine -> Socket a b -> Socket a b
placeSocket a = (center %~ csplace a) . (point %~ csplace a)

realizeSocket ::  Point -> ObjectLens a b -> Graph a -> ISocketObj b -> Maybe (Socket a b)
realizeSocket p f g  (ISocketObj io iso) = do
	(a,x) <- g ^. vertexes . at io 
	so <- x ^. f . at iso 
	return (placeSocket (glass p a) $ so)


realizeEdgeDep :: Graph a -> Edge ISocketObj Dep -> Point -> (Maybe  (Edge (Socket a) Dep))
realizeEdgeDep g (Edge x y) p = liftM2 Edge  
	(realizeSocket p objectOutputs g $ x)
	(realizeSocket p objectInputs g $ y)

realizeEdgeJumper :: Graph a -> Edge ISocketObj Jumper -> Point -> (Maybe  (Edge (Socket a) Jumper))
realizeEdgeJumper g (Edge x y) p = liftM2 Edge  
	(realizeSocket p objectControls g $ x)
	(realizeSocket p objectControls g $ y)
 

renderGraph :: (Monad m, Functor m) => RenderEdge  a m Dep -> RenderEdge  a m Jumper -> RenderObject a m -> Graph a -> Point -> m ()
renderGraph re rje ro g@(Graph sps es ejs as _) p = do
	forM_ (M.elems es) $ \e -> case realizeEdgeDep g e p of 
		Nothing -> error "lookup index failed in render graph"
		Just eas -> re eas 
	forM_ (M.elems ejs) $ \e -> case realizeEdgeJumper g e p of 
		Nothing -> error "lookup index failed in render graph"
		Just eas -> rje eas 
	forM_ sps $ \(a, x) -> ro x (glass p a) 

affineBack :: Affine -> Point -> Point
affineBack (Affine c k) x = ((x .-. c) ./. k) .+. (0.5,0.5)

socketDistance :: Point -> Socket a b -> Distance
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


----- vertexes -------------


sendToVertex :: (Functor m, Applicative m) => LensesOf a -> Point -> Graph a -> (Point -> a -> m a) -> m (Graph a)
sendToVertex k p g f = case nearestVertexes p g of 
	[] -> pure g
	io : _ -> let Just a = g ^? vertexes . at io . traverse . _1 in fmap (electricalO k io) $ 
		vertexes %%~ (ix io (_2 . object %%~ f (affineBack (glass p a) p))) $ g 


scaleXVertex :: Point -> Graph a -> (Double -> Double) -> Graph a
scaleXVertex p g f = case nearestVertexes p g of 
	[] -> g
	io : _ -> vertexes %~ (M.adjust (_1 . affineScale . _1 %~ f) io) $ g

scaleYVertex :: Point -> Graph a -> (Double -> Double) -> Graph a
scaleYVertex p g f = case nearestVertexes p g of 
	[] -> g
	io : _ -> vertexes %~ (M.adjust (_1 . affineScale . _2 %~ f) io) $ g


-- list all edges coming or going from a socket
socketEdges :: Graph a -> ISocketObj Input -> [IEdge Dep]
socketEdges g i  = map fst $ filter ((==i) . view (_2 . edgeEnd)) (M.assocs $ g ^. edgesDep) where

-- assigned aware edge romoval, unsafe in the edge index
removeEdgeDep ::  IEdge Dep -> Graph a -> Graph a
removeEdgeDep i g = let 
		e = (g ^. edgesDep) M.! i
		g' = edgesDep %~ M.delete i $ g
		ies = socketEdges g (e ^. edgeEnd) 
		fas = case ies of 
			[] -> const Nothing
			_ -> id
		in assigned . at (e ^. edgeEnd) %~ fas $ g'


removeEdgeJumper ::  IEdge Jumper -> Graph a -> Graph a
removeEdgeJumper i g = updateIsles $ edgesJumper %~ M.delete i $ g


-- all edges coming and going from something 
vertexEdges :: forall a . Graph a -> IObject -> ([IEdge Dep],[IEdge Jumper])
vertexEdges g io = let 
		f :: Edge ISocketObj b -> Bool
		f (Edge x y) = x ^. isobject == io || y ^. isobject  == io  
	in (M.keys . M.filter f $ g ^. edgesDep, M.keys . M.filter f $ g ^. edgesJumper)


-- | delete the nearest by socket vertex 
deleteVertex :: Point -> Graph a -> Graph a
deleteVertex p g = case nearestVertexes p g of
	[] -> g
	io: _ -> let 
		(xs,ys) =  vertexEdges g io
		in (vertexes . at io .~ Nothing) . flip (foldr removeEdgeDep) xs . foldr removeEdgeJumper g $ ys
 



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
{-sortEdges 
     :: Point
     -> M.Map (IEdge b) (Edge ISocketObj b)
     -> [IEdge b]
-}
sortEdges
  :: (s -> a -> Point -> Maybe (Edge (Socket a1) t))
     -> Lens' s (M.Map  b a)  -> Point -> s -> [(Distance,b)]

sortEdges r l c g = sortBy (comparing fst) . catMaybes $ do
			(ie,e) <- M.assocs $ g ^. l
			let 	f (Edge s1 s2) = (head $ sort [distance c (s1 ^. point), distance c (s2 ^. point)], ie)	
			return  . fmap f $ r g e c 

-- | delete the nearest edge to the given point
deleteEdge :: Point -> Graph m -> Graph m
deleteEdge p g = case sortEdges realizeEdgeDep edgesDep p g of
	[] -> case sortEdges realizeEdgeJumper edgesJumper p g of
		[] -> g
		io: _ -> removeEdgeJumper (snd io) g 
	io:_ ->  case sortEdges realizeEdgeJumper edgesJumper p g of
		[] -> removeEdgeDep (snd io) g
		io': _ -> case fst io > fst io' of
				True -> removeEdgeDep (snd io) g
				False -> removeEdgeJumper (snd io') g

		
addEdgeDep :: Edge ISocketObj Dep  -> Graph a -> Point -> Graph a
addEdgeDep e@(Edge es ee) g p = (assigned . at (ee) .~ (Just . sone $ es)) . (edgesDep . at (newKey $ g ^. edgesDep) .~ Just e) $ g where
	sone s = case fmap (\(SOutput _ _ n ) -> n) $ realizeSocket p objectOutputs g s of
		Nothing -> error "addEdge index error"
		Just x -> x

addEdgeJumper :: Edge ISocketObj Jumper  -> Graph a -> Point -> Graph a
addEdgeJumper e@(Edge  es ee) g p = updateIsles $ (edgesJumper . at (newKey $ g ^. edgesJumper) .~ Just e) $ g where

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
	conn (Edge i j) = (i ^. isobject,j ^. isobject)
	in  tsort $ map conn  (M.elems  $ g ^. edgesDep)

completeEdgeInput :: Eq (SocketName a) => ISocketObj Input -> Graph a -> Point  -> Graph a
completeEdgeInput j g p = 
	case realizeSocket p objectInputs g j of
		Nothing -> error "completeEdgeInput index error"
		Just (SInput _ _ ns) -> case filter (judgeOutputSockets p ns g j) $ nearestSockets objectOutputs p g of
			[] -> g
			i : _ -> 	let 	g' = addEdgeDep (Edge i j) g p in 
					case tsortg g' of
						Nothing -> g
						_ ->  g' --


 
completeEdgeOutput :: Eq (SocketName a) => ISocketObj Output -> Graph a -> Point -> Graph a
completeEdgeOutput j g p = 
	case realizeSocket p objectOutputs g j of
		Nothing -> error "completeEdgeOutput index error"
		Just (SOutput _ _ n ) -> case filter (judgeInputSockets p n g) $  nearestSockets objectInputs p g of
			[] -> g
			i : _ -> let 	g' = addEdgeDep (Edge j i) g p
				in case tsortg g' of
					Nothing -> g
					_ ->  g'

completeEdgeDuplex :: Eq (ControlName a) => ISocketObj Duplex -> Graph a -> Point -> Graph a
completeEdgeDuplex j g p = 
	case realizeSocket p objectControls g j of
		Nothing -> error "completeEdgeDuplex index error"
		Just (SControl _ _ n) -> case filter (judgeDuplexSockets p n g) $  nearestSockets objectControls p g of
			[] -> g
			i : _ -> addEdgeJumper (Edge j i) g p


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


support	edges remove real c1 c2 i g p = let
	Just (j@(Edge ji je)) = g ^. edges . at i
	Just (Edge si se) = real g j p
	in case socketDistance p si > socketDistance p se of
		True ->  Just $ c1 (ji) (remove i g)
		False ->  Just $ c2 (je) (remove i g) 

supportDep = support edgesDep removeEdgeDep realizeEdgeDep completeEdgeOutput completeEdgeInput

supportJumper = support edgesJumper removeEdgeJumper realizeEdgeJumper completeEdgeDuplex completeEdgeDuplex

-- | move one vertex of an edge
modifyEdge :: forall a . (Eq (SocketName a), Eq (ControlName a)) => Point -> Graph a -> Maybe (Point -> Graph a)
modifyEdge p g = case sortEdges realizeEdgeDep edgesDep p g of
	[] -> case sortEdges realizeEdgeJumper edgesJumper p g of
		[] -> Nothing
		io: _ -> supportJumper (snd io) g p
	io':_ ->  case sortEdges realizeEdgeJumper edgesJumper p g of
		[] -> supportDep (snd io') g p
		io: _ -> case fst io > fst io' of
			True -> supportDep (snd io') g p
			False -> supportJumper (snd io) g p
