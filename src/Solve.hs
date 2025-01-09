module Solve where

import           App               (MazeBuilder)
import           Control.Monad.RWS
import           Data.Foldable     (for_, minimumBy, traverse_)
import           Data.Function     (on)
import qualified Data.Map          as Map
import           Data.Maybe        (isJust, mapMaybe)
import           Maze              (Maze, Node (Node, value), NodeID, openPaths)

distance :: Monoid w => Int -> NodeID -> MazeBuilder c w Maze ()
distance d nid = do
  m <- get
  case Map.lookup nid m of
    Just node@(Node _ val _ _ _ _) ->
      if isJust val
        then return ()
        else do
          modify' $ setValue d nid
          let toUpdate = openPaths node
          traverse_ (distance (d + 1)) toUpdate
    Nothing -> return ()

-- update the value in the node within the map
setValue :: Int -> NodeID -> Maze -> Maze
setValue d = Map.adjust (\x -> x {value = Just d})

solveNode :: NodeID -> MazeBuilder c [NodeID] Maze ()
solveNode nid = do
  m <- get
  tell [nid]
  case Map.lookup nid m of
    Just node@(Node _ val _ _ _ _) ->
      case val of
        (Just v) -> do
          let paths = openPaths node
              next = findLowestPath v paths m
          for_ next solveNode
        Nothing -> pure ()
    Nothing -> pure ()

findLowestPath :: Int -> [NodeID] -> Maze -> Maybe NodeID
findLowestPath val ns m = do
  let vs = filter ((< val) . snd) $ mapMaybe (flip nodeValue m) ns
  if null vs
    then Nothing
    else Just $ fst (minimumBy (compare `on` snd) vs)
  where
    nodeValue i m = Map.lookup i m >>= (\v -> (,) i <$> v) . value

solve :: MazeBuilder c [NodeID] Maze ()
solve = do
  m <- get
  solveNode $ fst $ Map.findMax m
