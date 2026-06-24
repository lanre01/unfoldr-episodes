{-# LANGUAGE DerivingStrategies #-}
module Main where 
import Test.Tasty
import Test.Tasty.QuickCheck


import Dijkstra (Info, dijkstra)
import Dijkstra qualified
import Graph (Graph, Path, pathSource, pathDest)
import Graph qualified
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Function (on)
import Control.Monad
import Data.Map.Strict qualified as Map


-- Costs 

newtype Cost = Cost Int
    deriving (Show, Ord, Eq, Num)

instance Arbitrary Cost where 
    arbitrary       = Cost <$> choose (0 , 100)
    shrink (Cost c) = Cost <$> shrink c 


prop_costMonotone :: Cost -> Cost -> Property 
prop_costMonotone c1 c2 = 
    counterexample (show (c1 + c2)) $ 
      c1 <= c1 + c2 

data Vertex = A | B | C | D | E | Vertex Int 
       deriving stock (Show, Eq, Ord)
    
allVertices :: [Vertex]
allVertices = [A, B, C, D, E] ++ map Vertex [6 ..]


instance Arbitrary (Graph Vertex Cost) where 
    arbitrary = sized $ \sz -> do 
        numVertices <- choose (0, sz)
        let vertices = take numVertices allVertices
        if null vertices then 
            return $ Graph.fromEdges []
        else do
            numEdges <- choose (0, 10 * sz)
            fmap (Graph.fromEdges . nubBy ((==) `on` fst)) $
              replicateM numEdges $ do 
                a :: Vertex <- elements vertices
                b :: Vertex <- elements vertices 
                c :: Cost   <- arbitrary
                return ((a, b), c)
    
    shrink = map Graph.fromEdges . shrinkList shrinkCost . Graph.toEdges
        where
            shrinkCost :: ((Vertex, Vertex), Cost) -> [((Vertex, Vertex), Cost)]
            shrinkCost ((a, b), c) = map ((a, b), ) (shrink c)

prop_valid :: Graph Vertex Cost -> Property
prop_valid gr = gr `Graph.hasVertices` [A, B] ==>
      case Map.lookup B (Dijkstra.dist info) of 
        Nothing   -> counterexample ("discarding " ++ show gr) Discard
        Just cost ->  counterexample ("dijkstra reported cost" ++ show cost) $ 
            case Dijkstra.pathBetween info (A, B) of 
                Nothing   -> counterexample "cost but no path" False 
                Just path -> counterexample ("dijskra found path" ++ show path) $ 
                   case Graph.pathCostIfValid gr path of 
                    Nothing -> counterexample "path is invalid" False 
                    Just actualCost -> counterexample ("actual cost is " ++ show actualCost) $ 
                        cost === actualCost 

     where 
        info :: Info Vertex Cost 
        info = dijkstra (Graph.toDijkstra gr) A 

data PathInGraph = PathInGraph {
      pathIn :: Graph Vertex Cost 
    , path   :: Path Vertex 
    } deriving Show 

randomPathFrom :: forall v c. Eq v => Graph v c -> v -> Int -> Gen (Path v)
randomPathFrom gr = go 
     where 
        go :: v -> Int -> Gen (Path v)
        go v 0 = return (v :| [] )
        go v n = do 
            let nv = Graph.neighbours gr v 
            if null nv then 
                return (v :| [])
            else do 
                v' <- elements nv 
                NE.cons v <$> go v' (n - 1)

instance Arbitrary PathInGraph where 
    arbitrary = sized $ \sz -> do 
        gr   <- arbitrary `suchThat` (not . null . Graph.edges)
        src  <- elements (Graph.vertices gr)
        n    <- choose (0, sz)
        path <- randomPathFrom gr src n 
        return $ PathInGraph gr path

prop_shortest :: PathInGraph -> Property 
prop_shortest (PathInGraph pathIn path) = 
    case Map.lookup (pathDest path) (Dijkstra.dist info) of 
         Nothing   -> counterexample "No path found" False 
         Just cost -> counterexample (show (cost, actualCost)) $ 
            cost <= actualCost

    where 
        info :: Info Vertex Cost 
        info = dijkstra (Graph.toDijkstra pathIn) (pathSource path)

        actualCost :: Cost 
        actualCost = Graph.pathCost pathIn path 


main :: IO ()
main = 
    defaultMain $ 
      testGroup "Testing without reference"
        [
            testProperty "Cost is monotone" prop_costMonotone,
            testProperty "Valid graph" prop_valid,
            testProperty "Shortest path" prop_shortest
        ]