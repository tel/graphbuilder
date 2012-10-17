{- |
Module      : $Header$
Description : Graph construction language
Copyright   : (c) 2012 Joseph Abrahamson
License     : MIT
Maintainer  : Joseph Abrahamson <me@jspha.com>
Stability   : unstable
Portability : portable

@Data.Graph.Builder@ is a declarative, monadic language for building
graphs, especially trees. It supports both undirected ('UGraphBuilder'
and 'buildUGraph') and directed ('DiGraphBuilder', 'buildDiGraph')
graphs and allows for optional polymorphic labels on both the vertices
and the edges.

The types chosen are designed for easy import from the builder into
some other graph library such as FGL.

Here's an example of building a simple, unlabeled, digraph

@
g = runDiGraphBuilder $ do
  [a, b, c] <- vtcs_ 3
  link_ a b
  d <- vtc_ [b, c]
  link_ d a
  graph_
@

/Still to do/

* Graph unfolds

* Pointed graphs

* Graph gluing

-}

module Data.Graph.Builder (
    -- * Underlying types
  GraphBuilder,
  DiGraphBuilder, UGraphBuilder,
  Vertex, DiEdge, Edge,
  -- * Basic construction functions
  vtxF, vtxf, vtx, vtx_, vtcs,
  linkf, link, link_,
  label,
  graph, graph_,
  -- * Basic builder functions
  runGraphBuilder,
  runDiGraphBuilder,
  runUGraphBuilder
  ) where

import Data.Maybe
import Control.Monad
import Control.Arrow
import Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as M

data Vertex = Vertex { unVertex ::  Int } deriving (Show, Eq, Ord)
data DiEdge = DiEdge Vertex Vertex deriving (Show, Eq, Ord)
data Edge   = Edge Vertex Vertex deriving Show

-- | 'Edge's have a special 'Eq' and 'Ord' instances to indicate that
-- they're not directed: @(a,b) == (b,a)@.
instance Eq Edge where
  (Edge (Vertex a) (Vertex b)) == (Edge (Vertex a') (Vertex b')) =
    (a, b) == (a', b') || (a, b) == (b', a')

-- | 'Edge's have a special 'Eq' and 'Ord' instances to indicate that
-- they're not directed: @(a,b) == (b,a)@.
instance Ord Edge where
  ea@(Edge (Vertex a) (Vertex b)) `compare` eb@(Edge (Vertex a') (Vertex b')) =
    if ea == eb then EQ else (a, b) `compare` (a', b')
    

-- | 'Edge' generalization so that the same methods can work for both
-- directed and undirected graphs. The other way to do this would be
-- to have a sum type on the edges, allowing for graphs with both
-- directed and undirected edges, but I've only rarely seen those
-- occur.
class Ord e => EdgeType e where
  mkEdge :: Vertex -> Vertex -> e
  hd :: e -> Vertex
  tl :: e -> Vertex

instance EdgeType DiEdge where
  mkEdge = DiEdge
  hd (DiEdge a _) = a
  tl (DiEdge _ b) = b

instance EdgeType Edge where
  mkEdge = Edge
  hd (Edge a _) = a
  tl (Edge _ b) = b

-- | The 'GraphBuilder' monad is the central conceit of
-- 'Data.Graph.Builder'. It can be used to construct graphs,
-- especially trees, in a simple, declarative manner.
newtype GraphBuilder e a b x =
  GraphBuilder { unGB :: State (GraphState e a b) x }

type DiGraphBuilder = GraphBuilder DiEdge
type UGraphBuilder = GraphBuilder Edge

instance Functor (GraphBuilder e a b) where
  fmap f (GraphBuilder m) = GraphBuilder (fmap f m)

instance Monad (GraphBuilder e a b) where
  return a = GraphBuilder (return a)
  (GraphBuilder m) >>= f = GraphBuilder (m >>= unGB . f)


-- | Unwrap a 'GraphBuilder' monad.
runGraphBuilder :: EdgeType e => GraphBuilder e a b x -> x
runGraphBuilder = flip evalState g0 . unGB

-- | Unwrap a 'GraphBuilder' monad specifying *directed* edges. Useful
-- for when the type of the graph is never actually specified.
runDiGraphBuilder :: DiGraphBuilder a b x -> x
runDiGraphBuilder = runGraphBuilder

-- | Unwrap a 'GraphBuilder' monad specifying *undirected* edges. Useful
-- for when the type of the graph is never actually specified.
runUGraphBuilder :: UGraphBuilder a b x -> x
runUGraphBuilder = runGraphBuilder

data GraphState e a b =
  GraphState { vxs :: Map Vertex (Maybe a),
                 es :: Map e (Maybe b) }

modifyVxs :: (Map Vertex (Maybe a) -> Map Vertex (Maybe a))
             -> GraphBuilder e a b ()
modifyVxs f =
  GraphBuilder $ modify $ \ gs@(GraphState { vxs = vxs }) -> gs { vxs = f vxs }

modifyEs :: EdgeType e =>
            (Map e (Maybe b) -> Map e (Maybe b))
             -> GraphBuilder e a b ()
modifyEs f =
  GraphBuilder $ modify $ \ gs@(GraphState { es = es }) -> gs { es = f es }

unGS :: EdgeType e => GraphState e a b -> ([(Int, Maybe a)], [(Int, Int, Maybe b)])
unGS GraphState { vxs = vxs, es = es } =
  (map (first unVertex) $ M.toList vxs,
   map (\(e, l) -> (unVertex $ hd e, unVertex $ tl e, l)) $ M.toList es)

unGS_ :: EdgeType e => GraphState e a b -> ([Int], [(Int, Int)])
unGS_ GraphState { vxs = vxs, es = es } =
  (map (unVertex . fst) $ M.toList vxs,
   map (\(e, _) -> (unVertex $ hd e, unVertex $ tl e)) $ M.toList es)

g0 :: EdgeType e => GraphState e a b
g0 = GraphState { vxs = M.empty, es = M.empty }

-- | Creates a new vertex from a list of parents. The capital \"F\"
-- indicates that we must provide both the edge and vertex labeling
-- functions. These are local functions which can be used to propagate
-- graph labels.
vtxF :: EdgeType e
        => (Maybe a -> Maybe b)   -- ^ Link labeler
        -> ([Maybe a] -> Maybe a) -- ^ Vertex labeler
        -> [Vertex]               -- ^ List of parent vertices
        -> GraphBuilder e a b Vertex
vtxF ef lf parents = GraphBuilder $ do
  gs@(GraphState { vxs = vxs, es = es }) <- get
  let this = Vertex (M.size vxs + 1)
      vparents = map (fromJust . (`M.lookup` vxs)) parents
  put GraphState {
    -- Note the 'fromJust' here encodes the guarantee that we can't be
    -- using a vertex that doesn't exist
    vxs = M.insert this (lf vparents) vxs,
    es  = foldr (\(p, v) m ->
                  M.insert (mkEdge p this) (ef v) m) es (zip parents vparents)
    }
  return this

-- | Creates a new vertex from a list of parents. The lower-case \"f\"
-- indicates that we only need to provide the vertex labeling function
-- (see 'vtxF').
vtxf :: EdgeType e => ([Maybe a] -> Maybe a) -> [Vertex] -> GraphBuilder e a b Vertex
vtxf = vtxF (const Nothing)

vtx' :: EdgeType e => Maybe a -> [Vertex] -> GraphBuilder e a b Vertex
vtx' = vtxf . const

-- | Creates a new vertex from a list of parents. This is used for
-- constant labeled vertices.
vtx :: EdgeType e => a -> [Vertex] -> GraphBuilder e a b Vertex
vtx = vtx' . Just

-- | Creates a new vertex from a list of parents giving it no label.
vtx_ :: EdgeType e => [Vertex] -> GraphBuilder e a b Vertex
vtx_ = vtx' Nothing

-- | Create a number of new vertices with no parents.
vtcs :: EdgeType e => a -> Int -> GraphBuilder e a b [Vertex]
vtcs l = flip replicateM (vtx l [])

-- | Create a number of new, unlabled vertices with no parents.
vtcs_ :: EdgeType e => Int -> GraphBuilder e a b [Vertex]
vtcs_ = flip replicateM (vtx_ [])

-- | Link two vertices together, head to tail. The lower-case \"f\"
-- indicates that we can provide an edge labeling function.
linkf :: EdgeType e => (Maybe a -> Maybe a -> Maybe b)
         -> Vertex -> Vertex
         -> GraphBuilder e a b ()
linkf lf from to =
  GraphBuilder $ do
    gb@(GraphState { vxs = vxs, es = es }) <- get
    let edge = mkEdge from to
        vfrom = fromJust $ M.lookup from vxs
        vto   = fromJust $ M.lookup to   vxs
    put gb { es = M.insert edge (lf vfrom vto) es }
    return ()

link' :: EdgeType e => Maybe b -> Vertex -> Vertex -> GraphBuilder e a b ()
link' = linkf . const2
  where const2 a _ _ = a

-- | Link two vertices together, head to tail, and apply an edge label
link :: EdgeType e => b -> Vertex -> Vertex -> GraphBuilder e a b ()
link = link' . Just

-- | Link two vertices together, head to tail, with an unlabeled edge.
link_ :: EdgeType e => Vertex -> Vertex -> GraphBuilder e a b ()
link_ = link' Nothing

label' :: Maybe a -> Vertex -> GraphBuilder e a b ()
label' l v =
  modifyVxs $ M.insert v l

-- | Update the label of a given vertex.
label :: a -> Vertex -> GraphBuilder e a b ()
label = label' . Just

labelOf :: Vertex -> GraphBuilder e a b (Maybe a)
labelOf v = GraphBuilder $ do
  GraphState { vxs = vxs } <- get
  return $ fromJust $ M.lookup v vxs

-- | Return a structure isomorphic to the current graph.
graph :: EdgeType e => GraphBuilder e a b ([(Int, Maybe a)], [(Int, Int, Maybe b)])
graph = GraphBuilder (fmap unGS get)

-- | Return a structure isomorphic to the current graph, forgetting
-- the labels.
graph_ :: EdgeType e => GraphBuilder e a b ([Int], [(Int, Int)])
graph_ = GraphBuilder (fmap unGS_ get)