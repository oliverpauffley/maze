{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MazeShape where

import Control.Lens (makeLenses, view, (&), (.~), (^.))

import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.Random (MonadRandom, uniform)
import Control.Monad.Trans.RWS (gets)
import Data.Functor.Rep (Representable (..))
import Data.Map (Map, adjust)
import qualified Data.Map as Map hiding (mapMaybe)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

newtype NodeID = NodeID (Int, Int)
    deriving (Show, Eq)

instance Ord NodeID where
    compare (NodeID a) (NodeID b) = compare a b

data Path = Open | Closed
    deriving (Eq, Show)

openPath :: Path -> Path
openPath Open = Open
openPath Closed = Open

isOpen :: Path -> Bool
isOpen Open = True
isOpen Closed = False

data Edge e = Edge
    { _eID :: NodeID
    , _path :: e
    }
    deriving (Show, Eq)

makeLenses ''Edge

instance Functor Edge where
    fmap :: (a -> b) -> Edge a -> Edge b
    fmap f (Edge i e) = Edge i (f e)

type MEdge e = Maybe (Edge e)

type Position = (Int, Int)

type Maze d = Map NodeID (Node d (Maybe Int) Path)

-- | Partial function to get a node from an ID
getNode :: Maze d -> NodeID -> Node d (Maybe Int) Path
getNode m i = case Map.lookup i m of
    Just n -> n
    Nothing -> error $ "can't get node " <> show i

data Config = Config
    { diagramSize :: Double
    , mazeSize :: Int
    , solve :: Bool
    , withColor :: Bool
    , countDeadEnds :: Bool
    , debug :: Bool
    , fileName :: String
    , -- TODO fix mask
      -- , mask :: Maybe FilePath
      algorithm :: Algorithm
    , shape :: Shape
    }

data Algorithm
    = BinaryTree
    | Sidewinder
    | AldousBroder
    | Wilson
    | HuntKill
    | RecursiveBacktrack
    deriving (Eq, Show)

data Shape
    = Square
    | Hexagon
    deriving (Eq, Show)

-- | The main monad for the generate of mazes
type MazeBuilder s = RWST Config () s IO

-- | Run the builder to produce a maze
runBuilder :: MazeBuilder state a -> Config -> state -> IO (a, state)
runBuilder app c s = do
    (a, s', _) <- runRWST app c s
    return (a, s')

{- | A maze node with a set of possible connections and value. Each node has a node id that is used as the key to select the node. The directions are Maybe as on the edges or after masking has
been applied their may not be a connection.
-}
data Node d a e = Node
    { _nid :: NodeID
    , _value :: a
    , _directions :: d (MEdge e)
    }

makeLenses ''Node

type MazeNode d = Node d (Maybe Int) Path

randomNode :: MazeBuilder (Maze d) (Node d (Maybe Int) Path)
randomNode = uniform =<< gets Map.elems

{- | setter takes an index within a representable functor and updates that index's value
 whilst keeping the rest the same.
-}
setter :: (Representable d, Eq (Rep d)) => d a -> Rep d -> a -> d a
setter f updRep val =
    tabulate setter'
  where
    setter' rep = if rep == updRep then val else index f rep

{- | A set of directions where there is the concept of a reverse direction
 opposite (opposite d) == d
-}
class Opposite a where
    opposite :: a -> a

-- | list all connections from current node.
connections :: (Representable d, Bounded (Rep d), Enum (Rep d)) => Node d a e -> [(Edge e, Rep d)]
connections (Node _ _ dirs) = mapMaybe (\(medge, dir) -> (,dir) <$> medge) $ toList dirs

-- | list all nodes and filter with filtering function
connectionsWith ::
    (Representable d, Bounded (Rep d), Enum (Rep d)) => (Edge e -> Bool) -> Node d a e -> [(Edge e, Rep d)]
connectionsWith p n = filter (p . fst) $ connections n

openConnections :: (Representable d, Bounded (Rep d), Enum (Rep d)) => Node d a Path -> [NodeID]
openConnections n = (\(e, _) -> e ^. eID) <$> connectionsWith (\e -> (== Open) $ e ^. path) n

closedConnections :: (Representable d, Bounded (Rep d), Enum (Rep d)) => Node d a Path -> [(NodeID, Rep d)]
closedConnections n = (\(e, dir) -> (e ^. eID, dir)) <$> connectionsWith (\e -> (== Closed) $ e ^. path) n

-- | given a node, id and direction; apply a function on that node and dir and it's opposite
changeNodes ::
    (Representable d, Eq (Rep d), Opposite (Rep d)) =>
    Maze d ->
    NodeID ->
    Rep d ->
    (Maze d -> NodeID -> Rep d -> Maze d) ->
    Maze d
changeNodes m id dir f = f (f m id dir) id2 (opposite dir)
  where
    -- lookup the node directions from where are connecting from
    nodeDirs = view directions <$> Map.lookup id m
    -- find the node we are connecting to
    idNode Nothing = error "could not find node"
    idNode (Just dirs) = case index dirs dir of
        Nothing -> error "could not connect node, no node"
        Just edge -> edge ^. eID

    id2 = idNode nodeDirs

{- | connect the given node ID to the node in the direction given. Requires that you can find an opposite direction to connect the new node back to the starting node.
keeps the connection bidirectional.
-}
connectNodes :: (Representable d, Eq (Rep d), Opposite (Rep d)) => NodeID -> Rep d -> Maze d -> Maze d
connectNodes id dir m = changeNodes m id dir connectNode

-- | connect a node to the node in the given direction
connectNode :: (Representable d, Eq (Rep d)) => Maze d -> NodeID -> Rep d -> Maze d
connectNode m id dir = adjust (connectNode' dir) id m

-- | connectNode' takes a direction and a node and opens that connection within the nodes directions
connectNode' :: (Representable d, Eq (Rep d)) => Rep d -> Node d a Path -> Node d a Path
connectNode' dir node =
    let dirs = node ^. directions
        edge = index dirs dir
        newEdge = fmap openPath <$> edge
        dirs' = setter dirs dir newEdge
     in node & directions .~ dirs'

-- | removes a node from it's direction to the given ID so it is no longer connected with the given node
removeNodes :: (Representable d, Eq (Rep d), Opposite (Rep d)) => NodeID -> Rep d -> Maze d -> Maze d
removeNodes id dir m = changeNodes m id dir removeNode

removeNode :: (Representable d, Eq (Rep d)) => Maze d -> NodeID -> Rep d -> Maze d
removeNode m id dir = adjust (removeNode' dir) id m

removeNode' :: (Representable d, Eq (Rep d)) => Rep d -> Node d a Path -> Node d a Path
removeNode' dir node =
    let dirs = node ^. directions
        dirs' = setter dirs dir Nothing
     in node & directions .~ dirs'

-- | randomDirection picks a random direction from the node provided that exists. It does not check if the path is currently open or closed
randomDirection :: (MonadRandom m, Representable d, Bounded (Rep d), Enum (Rep d)) => Node d a e -> m (Rep d, Edge e)
randomDirection n =
    randomDirectionWith n (const True)

-- | get a random direction after filtering the edges with the given predicate.
randomDirectionWith ::
    (MonadRandom m, Representable d, Bounded (Rep d), Enum (Rep d)) => Node d a e -> (Edge e -> Bool) -> m (Rep d, Edge e)
randomDirectionWith n p =
    uniform . filterEdge p . map swap . toList $ n ^. directions
  where
    filterEdge _ [] = []
    filterEdge p ((_, Nothing) : xs) = filterEdge p xs
    filterEdge p ((dir, Just e) : xs)
        | p e = (dir, e) : filterEdge p xs
        | otherwise = filterEdge p xs

-- | turn a representable functor into a list of each part of the functor with it's index value
toList :: (Representable d, Bounded (Rep d), Enum (Rep d)) => d b -> [(b, Rep d)]
toList r =
    let range = [minBound .. maxBound]
     in map (\dir -> (index r dir, dir)) range

-- | Pointwise addition
(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

infixl 6 .+.
