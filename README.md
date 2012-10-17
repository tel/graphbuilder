# Data.Graph.Builder

"Data.Graph.Builder" is a declarative, monadic language for building
graphs, especially trees. It supports both undirected ('UGraphBuilder'
and 'buildUGraph') and directed ('DiGraphBuilder', 'buildDiGraph')
graphs and allows for optional polymorphic labels on both the vertices
and the edges.

The types chosen are designed for easy import from the builder into
some other graph library such as FGL or Graphviz.

Here's an example of building a simple, unlabeled, digraph

```haskell
g = runDiGraphBuilder $ do
  [a, b, c] <- vtcs_ 3
  link_ a b
  d <- vtc_ [b, c]
  link_ d a
  graph_
```

## Still to do

- Graph unfolds
- Pointed graphs
- Graph gluing
