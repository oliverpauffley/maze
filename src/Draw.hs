{-# LANGUAGE FlexibleContexts #-}

module Draw (mazeToDiagram)
where

import Data.Map (foldrWithKey)
import qualified Data.Map as Map
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (
    Diagram,
    moveTo,
    scale,
    strokeLocTrail,
    text,
    (#),
 )
import MazeShape (
    EdgeState (Closed, Open),
    GridShape (neighbor, toShape),
    Maze (Maze),
    NodeShape (NodeShape),
    edgeKey,
 )

type Solution coord = [coord]
type DeadEnds coord = [coord]

mazeToDiagram :: (Show coord, GridShape coord, Ord coord) => Maze coord () -> Diagram B
mazeToDiagram (Maze nodes edges) = foldMap drawNode (Map.keys nodes)
  where
    drawNode n =
        let (NodeShape center edgeLines) = toShape n
         in foldrWithKey (walls n) mempty edgeLines <> (text (show n) # scale 0.2 # moveTo center)

    -- if the edge is an open passage we should not draw a wall. Otherwise draw it!
    walls n dir trail acc = case isOpen n dir of
        Open -> acc
        Closed -> acc <> strokeLocTrail trail

    -- check for the node and direction if the neighbor is present there and open.
    -- anything else return closed.
    isOpen n dir = case neighbor n dir of
        Just n' | Map.member n' nodes && Map.lookup (edgeKey n n') edges == Just Open -> Open
        _otherwise -> Closed

-- to test ghci> :main -o test.svg -w 400
-- main :: IO ()
-- main =
--     mainWith $
--         mazeToDiagram $
--             connectEdge (Sigma (0, 0)) (Sigma (1, 0)) $
--                 connectEdge (Sigma (1, 1)) (Sigma (1, 0)) $
--                     newSigmaGrid 2

--     drawMaze :: Solution -> DeadEnds -> MazeBuilder (Maze a) (Diagram B)
--     drawMaze solution deadEnds = do
--         maze <- get
--         rColour <- getRandomColor
--         mazePicture <- mapM (drawNode rColour) (Map.elems maze)
--         solutionPath <- drawSolution (getNodes solution maze)
--         deadEndCount <- drawDeadEnds deadEnds
--         return $ vsep 1 [solutionPath <> position mazePicture, deadEndCount] # frame 0.3

--     nodeToPoint :: Node a v p -> Point V2 Double
--     drawEdges :: a (MEdge Path) -> [(MEdge Path, [Point V2 Double])]
--     colorNode :: Colour Double -> Maybe Int -> MazeBuilder (Maze a) (Diagram B)

-- drawNode ::
--     (DrawMaze a) => Colour Double -> Node a (Maybe Int) Path -> MazeBuilder (Maze a) (Point V2 Double, Diagram B)
-- drawNode col node = do
--     debugLabels <- labels (node ^. value)
--     colors <- colorNode col (node ^. value)
--     let es = edges (node ^. directions)
--         nodePos = nodeToPoint node
--         ds = [es, debugLabels, colors]
--         posDs = zip (repeat (p2 (0.0, 0.0))) ds
--     return (nodePos, position posDs)

-- getNodes :: [NodeID] -> Maze d -> [Node d (Maybe Int) Path]
-- getNodes ns maze = map (getNode maze) ns

-- drawSolution :: (DrawMaze d) => [Node d (Maybe Int) Path] -> MazeBuilder m (Diagram B)
-- drawSolution solution = do
--     Config{..} <- ask
--     if solve
--         then
--             return $
--                 strokePath (fromVertices $ map nodeToPoint solution)
--                     # lc red
--                     # lw 2
--                     # lineCap LineCapRound
--                     # lineJoin LineJoinRound
--         else return mempty

-- drawDeadEnds :: [NodeID] -> MazeBuilder s (Diagram B)
-- drawDeadEnds ns = do
--     Config{..} <- ask
--     if countDeadEnds
--         then return (text ("Dead ends: " <> show (length ns)))
--         else return mempty

-- getRandomColor :: (Random a, MonadIO m, Fractional a) => m (Colour a)
-- getRandomColor = do
--     r <- randomIO
--     g <- randomIO
--     b <- randomIO
--     return $ rgb r g b

-- labels :: (Show a) => Maybe a -> MazeBuilder m (Diagram B)
-- labels Nothing = return mempty
-- labels (Just a) = do
--     debug <- asks debug
--     if not debug
--         then return mempty
--         else
--             return $ scale 0.5 (text (show a))

-- -- | generically draw all edges
-- edges :: (DrawMaze d) => d (MEdge Path) -> Diagram B
-- edges = mconcat . map (uncurry drawEdge) . drawEdges # lwO 10 # lineCap LineCapRound # lineJoin LineJoinRound

-- drawEdge :: (TrailLike t, Monoid t) => Maybe (Edge Path) -> [Point (V t) (N t)] -> t
-- drawEdge Nothing p = fromVertices p
-- drawEdge (Just (Edge _ e)) p = case e of
--     Open -> mempty
--     Closed -> fromVertices p
