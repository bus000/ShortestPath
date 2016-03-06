{- |
 - Module      :  Dijkstra
 - Description :  Find shortest paths in graphs.
 - Copyright   :  (c) Magnus Stavngaard
 - License     :  <license>
 -
 - Maintainer  :  magnus@stavngaard.dk
 - Stability   :  experimental
 - Portability :  portable
 -
 - Implementation of Dijkstras algorithm for finding the shortest path in a
 - graph.
 -}
module Dijkstra ( Edge(..) , Vertex(..), Graph(..),
    Dijkstra.fromList, addVertex, dijkstra, showPath )
    where

import InfNum
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad.State as State
import Data.Array

-- | Represents a connection between two vertices in a graph.
data Edge a b = Edge
    { start :: Vertex a b -- ^ The start of the edge.
    , end   :: Vertex a b -- ^ The end of the edge.
    , cost  :: b          -- ^ The cost of using the edge.
    } deriving (Show)

-- | Represents a vertex in a graph.
data Vertex a b = Vertex
    { vertexID    :: a          -- ^ Unique ID of vertex.
    , vertexEdges :: [Edge a b] -- ^ List of outgoing edges.
    } deriving (Show)

instance (Eq a) => Eq (Vertex a b) where
    v1 == v2 = vertexID v1 == vertexID v2

instance (Ord a) => Ord (Vertex a b) where
    compare v1 v2 = compare (vertexID v1) (vertexID v2)

-- | Represents a mathematical graph.
data Graph a b = Graph
    { graphVertices :: [Vertex a b] -- ^ List of all vertices in the graph.
    , graphEdges    :: [Edge a b]   -- ^ List of all edges in the graph.
    } deriving (Show)

type Path a b = [Vertex a b]

-- | Construct a new graph from a list of vertices.
fromList :: (Eq a, Num b)
    => [Vertex a b] -- ^ List of vertices in graph.
    -> Graph a b    -- ^ New graph.
fromList vertices = Graph
    { graphEdges = getEdges vertices
    , graphVertices = vertices
    }
  where
    getEdges [] = []
    getEdges (x:xs) = (vertexEdges x) ++ (getEdges vertices)

-- | Add a new vertex to an existing graph.
addVertex :: Graph a b -- ^ Graph to insert vertex in.
    -> Vertex a b      -- ^ Vertex to insert in graph.
    -> Graph a b       -- ^ Resulting graph with new vertex.
addVertex graph vertex =
    let vertexes = graphVertices graph
        vertexes' = vertex : vertexes
    in Graph vertexes' (graphEdges graph)

-- | Find a vertex in a list of vertices from its identification.
findVertex :: (Eq a, Num b)
    => a                  -- ^ The identification of the vertex.
    -> [Vertex a b]       -- ^ List of vertices.
    -> Maybe (Vertex a b) -- ^ Nothing if not found else Just the found vertex.
findVertex ident = List.find (\x -> (vertexID) x == ident)

-- | Creates a string from a path.
showPath :: (Show a)
    => Path a b -- ^ The path to show.
    -> String   -- ^ The string representing the path.
showPath [] = []
showPath (vertex:vertices) = (show $ vertexID vertex) ++ ", " ++
    showPath vertices

-- | Find a path through a graph using Dijkstra's algorithm.
dijkstra :: (Eq a, Ord a, Ix a, Show a, Num b, Ord b)
    => a                -- ^ Identification of start vertex.
    -> a                -- ^ Identification of end vertex.
    -> Graph a b        -- ^ Graph to find path through.
    -> Maybe (Path a b) -- ^ Maybe find path through graph.
dijkstra vertex1 vertex2 graph = do
    let vertices = graphVertices graph
    start <- findVertex vertex1 vertices
    end <- findVertex vertex2 vertices
    let minvertex = minimum vertices
        maxvertex = maximum vertices
        dists = array (vertexID minvertex, vertexID maxvertex) $ map (\x ->
            if (vertexID x) == vertex1 then
                    (vertexID x, Number 0)
                else
                    (vertexID x, PosInf)) vertices
        prevs = array (vertexID minvertex, vertexID maxvertex) $ map (\x ->
            (vertexID x, Nothing)) vertices
        unvisited = graphVertices graph
        unvisited' = List.delete start unvisited

        (dists', prevs') = dijkstra' dists prevs unvisited' (Just start)

    getpath vertex1 vertex2 prevs'

  where
    dijkstra' :: (Eq a, Ord a, Ix a, Num b, Ord b)
        => Array a (InfNum b)
        -> Array a (Maybe (Vertex a b))
        -> [Vertex a b]
        -> Maybe (Vertex a b)
        -> (Array a (InfNum b), Array a (Maybe (Vertex a b)))
    dijkstra' dists prevs unvisited Nothing = (dists, prevs)
    dijkstra' dists prevs unvisited (Just current) =
        let edges = vertexEdges current
            (prevs', dists') = updateDistances edges current dists prevs
        in if unvisited == [] then
                (dists', prevs')
            else
                let (min, unvisited') = removeMinVertex unvisited dists'
                in dijkstra' dists' prevs' unvisited' $ Just min

    updateDistances :: (Eq a, Ord a, Ix a, Num b, Eq b, Ord b)
        => [Edge a b]
        -> Vertex a b
        -> Array a (InfNum b)
        -> Array a (Maybe (Vertex a b))
        -> (Array a (Maybe (Vertex a b)), Array a (InfNum b))
    updateDistances [] current dists prevs = (prevs, dists)
    updateDistances (edge:edges) current dists prevs =
        let currentID = vertexID current
            newdist = (Number $ cost edge) + (dists ! currentID)
            endID = vertexID (end edge)
        in if newdist < (dists ! endID) then
            let prevs' = prevs // [(endID, Just current)]
                dists' = dists // [(endID, newdist)]
            in updateDistances edges current dists' prevs'
        else
            updateDistances edges current dists prevs

    getpath :: (Eq a, Ord a, Ix a)
        => a
        -> a
        -> Array a (Maybe (Vertex a b))
        -> Maybe (Path a b)
    getpath start end prevs = getpath' start end prevs $ Just []

    getpath' :: (Eq a, Ord a, Ix a)
        => a
        -> a
        -> Array a (Maybe (Vertex a b))
        -> Maybe (Path a b)
        -> Maybe (Path a b)
    getpath' _ _ _ Nothing = Nothing
    getpath' start end prevs (Just path)
        | start == end = return path
        | otherwise    =
            let prev = prevs ! end
            in case prev of
                Nothing -> Nothing
                Just prevVertex -> let prevID = vertexID prevVertex
                    in getpath' start prevID prevs $ return (prevVertex : path)

    removeMinVertex :: (Eq a, Ord a, Ix a, Num b, Ord b)
        => [Vertex a b]
        -> Array a (InfNum b)
        -> (Vertex a b, [Vertex a b])
    removeMinVertex vertices dists =
        let min = getMinVertex vertices dists (head vertices)
        in (min, List.delete min vertices)

    getMinVertex :: (Eq a, Ord a, Ix a, Num b, Ord b)
        => [Vertex a b]
        -> Array a (InfNum b)
        -> Vertex a b
        -> Vertex a b
    getMinVertex [] dists min = min
    getMinVertex (vertex:vertices) dists min =
        if dists ! (vertexID vertex) < (dists ! (vertexID min))
        then getMinVertex vertices dists vertex
        else getMinVertex vertices dists min
