{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Node where

import Control.Monad.Random
import Data.Distributive (Distributive (distribute))
import Data.Functor.Rep
import Data.Graph (components)
import Data.Map hiding (filter, map, toList)
import Data.Maybe (catMaybes)
import Maze hiding (East, Maze, North, South, West)

type MEdge e = Maybe (Edge e)

data Node' d a e = Node'
    { _nid :: NodeID
    , _value :: a
    , _directions :: d (MEdge e)
    }

updateNodes :: (Representable d) => Node' d a e -> (d (MEdge e) -> Rep d -> MEdge e) -> Node' d a e
updateNodes (Node' i val dirs) f = Node' i val (tabulate $ f dirs)

setNodes :: (Representable f) => (f a -> t) -> (Rep f -> a) -> t
setNodes fn f = fn (tabulate f)

-- | randomDirection picks a random direction from the node provided that exists. It does not check if the path is currently open or closed
randomDirection :: (MonadRandom m, Representable f, Bounded (Rep f), Enum (Rep f)) => Node' f a e -> m (Edge e)
randomDirection n =
    randomDirectionWith n (const True)

-- | get a random direction after filtering the edges with the given predicate.
randomDirectionWith :: (MonadRandom m, Representable f, Bounded (Rep f), Enum (Rep f)) => Node' f a e -> (Edge e -> Bool) -> m (Edge e)
randomDirectionWith n p =
    uniform . filter p . catMaybes . toList $ _directions n

type Maze d = Map NodeID (Node' d (Maybe Int) Path)

type SquareGrid = Maze Cardinal

data CardinalDir = North | South | East | West
    deriving (Enum, Eq, Ord, Bounded)

class FromCardinalDir a where
    fromCardinalDir ::  CardinalDir -> a

data Cardinal a = Cardinal
    { _north :: a
    , _east :: a
    , _south :: a
    , _west :: a
    }
    deriving (Eq, Show)

instance Functor Cardinal where
    fmap f (Cardinal n e s w) = Cardinal (f n) (f e) (f s) (f w)

toList :: (Representable f, Bounded (Rep f), Enum (Rep f)) => f b -> [b]
toList r =
    let range = [minBound .. maxBound]
     in map (index r) range

-- >>> distribute (Just (Cardinal 1 2 3 4))
-- Cardinal {_north = Just 1, _east = Just 2, _south = Just 3, _west = Just 4}
instance Distributive Cardinal where
    distribute m = Cardinal (_north <$> m) (_east <$> m) (_south <$> m) (_west <$> m)

-- >>> index (Cardinal 1 2 3 4) South
-- 3
-- >>> tabulate (const 1) :: Cardinal Int
-- Cardinal {_north = 1, _east = 1, _south = 1, _west = 1}
instance Representable Cardinal where
    type Rep Cardinal = CardinalDir

    index :: Cardinal a -> (Rep Cardinal -> a)
    index (Cardinal n _ _ _) North = n
    index (Cardinal _ e _ _) East = e
    index (Cardinal _ _ s _) South = s
    index (Cardinal _ _ _ w) West = w

    tabulate :: (Rep Cardinal -> a) -> Cardinal a
    tabulate b = Cardinal (b North) (b East) (b South) (b West)

instance Applicative Cardinal where
    pure :: a -> Cardinal a
    pure a = Cardinal a a a a

    (<*>) :: Cardinal (a -> b) -> Cardinal a -> Cardinal b
    (<*>) (Cardinal f1 f2 f3 f4) (Cardinal a b c d) = Cardinal (f1 a) (f2 b) (f3 c) (f4 d)

-- | for any representable type with an index that can be cast to a `Cardinal` direction, get the north and east components
northEastDirections :: (FromCardinalDir (Rep f), Representable f) => Node' f a e -> [MEdge e]
northEastDirections n = [dir North, dir East]
  where
    dir d = index (_directions n) (fromCardinalDir d)
