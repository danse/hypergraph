import Data.Hypergraph
--import Data.Map (Map)
--import Data.Set (Set)
import Test.Hspec
--import Test.QuickCheck

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "Hypergraph" $ do
    describe "edgeSets" $
      it "works in a simple case" $
        let
          graph = fromLists [
            (1 :: Int, ['a', 'b']),
            (2, ['a']),
            (3, ['a', 'b']),
            (4, ['b'])]
          expected = [
            (Set.fromList ['a'], Set.fromList [2]),
            (Set.fromList ['a', 'b'], Set.fromList [1, 3]),
            (Set.fromList ['b'], Set.fromList [4])]
        in edgeSets graph `shouldBe` expected
