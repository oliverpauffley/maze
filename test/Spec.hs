import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as Map
import           Maze
import           Sidewinder                       (linkedCells)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "maze" $ do
    it "creates a new blank maze" $
      newMaze 2
        `shouldBe` Map.fromList
          [(NodeID (0, 0), Node {nid = NodeID (0, 0), value = (), north = Just (Edge {nodeID = NodeID (0, 1), e = Closed}), south = Nothing, east = Just (Edge {nodeID = NodeID (1, 0), e = Closed}), west = Nothing}), (NodeID (1, 0), Node {nid = NodeID (1, 0), value = (), north = Just (Edge {nodeID = NodeID (1, 1), e = Closed}), south = Nothing, east = Nothing, west = Just (Edge {nodeID = NodeID (0, 0), e = Closed})}), (NodeID (0, 1), Node {nid = NodeID (0, 1), value = (), north = Nothing, south = Just (Edge {nodeID = NodeID (0, 0), e = Closed}), east = Just (Edge {nodeID = NodeID (1, 1), e = Closed}), west = Nothing}), (NodeID (1, 1), Node {nid = NodeID (1, 1), value = (), north = Nothing, south = Just (Edge {nodeID = NodeID (1, 0), e = Closed}), east = Nothing, west = Just (Edge {nodeID = NodeID (0, 1), e = Closed})})]

    it "determines the direction between two nodes" $
      [ nodesDirection (NodeID (0, 0)) (NodeID (1, 0)),
        nodesDirection (NodeID (1, 0)) (NodeID (0, 0)),
        nodesDirection (NodeID (0, 0)) (NodeID (0, 1)),
        nodesDirection (NodeID (3, 2)) (NodeID (3, 1))
      ]
        `shouldBe` [East, West, North, South]

    it "compares node IDs correctly" $
      compare (NodeID (0, 0)) (NodeID (0, 1)) `shouldBe` LT
    it "compares node IDs correctly" $
      compare (NodeID (4, 0)) (NodeID (0, 1)) `shouldBe` LT
    it "compares node IDs correctly" $
      compare (NodeID (4, 4)) (NodeID (4, 4)) `shouldBe` EQ
    it "compares node IDs correctly" $
      compare (NodeID (4, 2)) (NodeID (8, 1)) `shouldBe` GT

    it "connects two nodes -> east" $ do stateTest

    it "returns linked cells" $
      linkedCells (Map.fromList [(NodeID (0, 0), Node {nid = NodeID (0, 0), value = (), north = Just (Edge {nodeID = NodeID (0, 1), e = Closed}), south = Nothing, east = Just (Edge {nodeID = NodeID (1, 0), e = Open}), west = Nothing}), (NodeID (1, 0), Node {nid = NodeID (1, 0), value = (), north = Just (Edge {nodeID = NodeID (1, 1), e = Closed}), south = Nothing, east = Nothing, west = Just (Edge {nodeID = NodeID (0, 0), e = Open})}), (NodeID (0, 1), Node {nid = NodeID (0, 1), value = (), north = Nothing, south = Just (Edge {nodeID = NodeID (0, 0), e = Closed}), east = Just (Edge {nodeID = NodeID (1, 1), e = Closed}), west = Nothing}), (NodeID (1, 1), Node {nid = NodeID (1, 1), value = (), north = Nothing, south = Just (Edge {nodeID = NodeID (1, 0), e = Closed}), east = Nothing, west = Just (Edge {nodeID = NodeID (0, 1), e = Closed})})]) (Node (NodeID (1, 0)) () Nothing Nothing Nothing (Just (Edge (NodeID (0, 0)) Open)))
        `shouldBe` [Node {nid = NodeID (0,0), value = (), north = Just (Edge {nodeID = NodeID (0,1), e = Closed}), south = Nothing, east = Just (Edge {nodeID = NodeID (1,0), e = Open}), west = Nothing}]

stateTest :: IO ()
stateTest = do
  maze <-
    execStateT (connect (NodeID (0, 0)) (NodeID (1, 0))) (newMaze 2)
  maze `shouldBe` Map.fromList [(NodeID (0, 0), Node {nid = NodeID (0, 0), value = (), north = Just (Edge {nodeID = NodeID (0, 1), e = Closed}), south = Nothing, east = Just (Edge {nodeID = NodeID (1, 0), e = Open}), west = Nothing}), (NodeID (1, 0), Node {nid = NodeID (1, 0), value = (), north = Just (Edge {nodeID = NodeID (1, 1), e = Closed}), south = Nothing, east = Nothing, west = Just (Edge {nodeID = NodeID (0, 0), e = Open})}), (NodeID (0, 1), Node {nid = NodeID (0, 1), value = (), north = Nothing, south = Just (Edge {nodeID = NodeID (0, 0), e = Closed}), east = Just (Edge {nodeID = NodeID (1, 1), e = Closed}), west = Nothing}), (NodeID (1, 1), Node {nid = NodeID (1, 1), value = (), north = Nothing, south = Just (Edge {nodeID = NodeID (1, 0), e = Closed}), east = Nothing, west = Just (Edge {nodeID = NodeID (0, 1), e = Closed})})]
