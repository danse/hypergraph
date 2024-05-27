module Data.Hypergraph
  (
    Hypergraph(Hypergraph),
    nodes,
    edges,
    fromEdges,
    edgeSets
  )
  where

import Data.Map (Map)
--import Data.Function (on)
import Data.Set (Set)
import Lens.Micro (Lens')
import Lens.Micro.Extras (view)

--import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens

data (Ord n) => Hypergraph e n =
  Hypergraph { _nodes :: (Map n (Set e)) } deriving (Show)

instance (Ord e, Ord n) => Semigroup (Hypergraph e n) where
  h1 <> h2 =
    Hypergraph $ Map.unionWith Set.union (view nodes h1) (view nodes h2)

nodes :: (Ord n) => Lens' (Hypergraph e n) (Map n (Set e))
nodes = Lens.lens _nodes (const Hypergraph)

-- | Requires mapset inversions
edges :: (Ord n, Ord e) => Lens' (Hypergraph e n) (Map e (Set n))
edges = Lens.lens toEdges (const fromEdges)
  where
    toEdges :: (Ord e, Ord n) => Hypergraph e n -> Map e (Set n)
    toEdges = inverseMapSet . view nodes

-- | Because we don't have isos
fromEdges :: (Ord e, Ord n) => Map e (Set n) -> Hypergraph e n
fromEdges = Hypergraph . inverseMapSet

inverseMapSet :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
inverseMapSet = Map.foldrWithKey folding Map.empty
  where
    folding :: (Ord a, Ord b) => a -> Set b -> Map b (Set a) -> Map b (Set a)
    folding a s m =
      let insertElement b m' =
            Map.insertWith Set.union b (Set.singleton a) m'
      in foldr insertElement m s

-- | `edgeSets` collects sets of edges sharing sets of nodes
edgeSets :: (Ord n, Ord e) => Hypergraph e n -> [(Set e, Set n)]
edgeSets =
  Map.toList . Map.foldrWithKey folding Map.empty . view nodes
  where
    folding
      :: (Ord e, Ord n)
      => n -> Set e -> Map (Set e) (Set n) -> Map (Set e) (Set n)
    folding node edgeSet =
      Map.insertWith Set.union edgeSet (Set.singleton node)

fromLists :: (Ord n, Ord e) => [(n, [e])] -> Hypergraph e n
fromLists = Hypergraph . Map.fromList . fmap (Lens.over Lens._2 Set.fromList)

{-
-- considering a 2-uniform like:

data Graph n = Graph [Edge n] deriving Show
data Edge  n = Edge (n, n) deriving Show -- undirected

-- these could be handy from the 2-uniform
data Linked n = Linked (Map n (Set n)) deriving Show
type Walks n = Tree n

-- an hypergraph could be transformed into it by:

-- | requires combinations causing an explosion of edges that can be calculated with the binomial coefficient
2-uniform :: Hypergraph e n -> Graph n
nodify :: (e -> n) -> Hypergraph e n -> Graph n
-}
