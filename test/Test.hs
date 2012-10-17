module Main where

import Data.Graph.Builder

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Monadic" [
     testProperty "Build from random tree" prop_canBuild
     ]
  ]

data GF = GF { count :: Int, edges :: [(Int, Int)] } deriving (Show, Eq)

instance Arbitrary GF where
  arbitrary = do
    n <- choose (1, 50)
    xs <- forM (enumFromTo 1 n) $ \i -> 
      forM (enumFromTo (i+1) n) $ \j -> do
        b <- frequency [(10, return False), (1, return True)]
        return (i, j, b)
    return GF { count = n,
                edges = map (\ (a, b, _) -> (a, b))
                        $ filter (\ (_, _, b) -> b)
                        $ concat xs }

-- This could be much more easily done if we restructured GraphBuilder
-- to be a monad transformer, then we could just wrap it around a
-- StateT monad and it'd be grand. Instead we have to generate and
-- carry about this Data.Map

prop_canBuild :: GF -> Property
prop_canBuild gf@(GF { count = count, edges = edges }) =
  monadic runDiGraphBuilder $ do
    let vx = enumFromTo 1 count
    vtxMap <- run
              $ liftM M.fromList
              $ mapM (\i -> do
                        x <- vtx_ []
                        return (i, x))
              vx
    run $ mapM_ (\(a,b) ->
                  link_
                  (fromJust $ M.lookup a vtxMap)
                  (fromJust $ M.lookup b vtxMap)) edges
    (newVx, newE) <- run graph_
    assert $ S.fromList newVx == S.fromList vx
    assert $ S.fromList newE  == S.fromList edges
    return undefined