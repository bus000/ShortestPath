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
    Dijkstra.fromList, addVertex, dijkstra )
    where

import InfNum
import qualified Data.List as List
import qualified Data.Map as Map

-- | Represents a connection between two vertices in a graph.
data Edge a = Edge
    { start :: Vertex a -- ^ The start of the edge.
    , end   :: Vertex a -- ^ The end of the edge.
    , cost  :: Integer  -- ^ The cost of using the edge.
    } deriving (Show)

-- | Represents a vertex in a graph.
data Vertex a = Vertex
    { vertexID    :: a        -- ^ Unique ID of vertex.
    , vertexEdges :: [Edge a] -- ^ List of outgoing edges.
    } deriving (Show)

instance (Eq a) => Eq (Vertex a) where
    v1 == v2 = vertexID v1 == vertexID v2

-- | Represents a mathematical graph.
data Graph a = Graph
    { graphVertices :: [Vertex a] -- ^ List of all vertices in the graph.
    , graphEdges    :: [Edge a]   -- ^ List of all edges in the graph.
    } deriving (Show)

type Path a = [Vertex a]

-- | Construct a new graph from a list of vertices.
fromList :: Eq a
    => [Vertex a] -- ^ List of vertices in graph.
    -> Graph a    -- ^ New graph.
fromList vertices = Graph
    { graphEdges = getEdges vertices
    , graphVertices = vertices
    }
  where
    getEdges [] = []
    getEdges (x:xs) = (vertexEdges x) ++ (getEdges vertices)

-- | Add a new vertex to an existing graph.
addVertex :: Graph a -- ^ Graph to insert vertex in.
    -> Vertex a -- ^ Vertex to insert in graph.
    -> Graph a  -- ^ Resulting graph with new vertex.
addVertex graph vertex =
    let vertexes = graphVertices graph
        vertexes' = vertex : vertexes
    in Graph vertexes' (graphEdges graph)

-- | Find a vertex in a list of vertices from its identification.
findVertex :: (Eq a)
    => a                -- ^ The identification of the vertex.
    -> [Vertex a]       -- ^ List of vertices.
    -> Maybe (Vertex a) -- ^ Nothing if not found else Just the found vertex.
findVertex ident = List.find (\x -> (vertexID) x == ident)

-- | Find a path through a graph using Dijkstra's algorithm.
dijkstra :: (Eq a, Ord a)
    => a                       -- ^ Identification of start vertex.
    -> a                       -- ^ Identification of end vertex.
    -> Graph a                 -- ^ The graph to search for a path in.
    -> Maybe (Path a, Integer) -- ^ The path from start to end and the length.
dijkstra vertex1 vertex2 graph = do
    start <- findVertex vertex1 $ graphVertices graph
    end <- findVertex vertex2 $ graphVertices graph
    let distances = Map.singleton vertex1 (0, Nothing)
        unvisited = List.filter (\x -> not (vertex1 == vertexID x))
            (graphVertices graph)
        distances' = dijkstra' unvisited start distances
    path <- getpath start end distances'
    (dist, _) <- Map.lookup vertex2 distances'
    Just (path, dist)

  where
    dijkstra' :: (Eq a, Ord a) => [Vertex a] -> Vertex a ->
        Map.Map a (Integer, Maybe (Vertex a)) ->
        Map.Map a (Integer, Maybe (Vertex a))
    dijkstra' [] current distances = distances
    dijkstra' unvisited current distances =
        let distances' = newDistances (vertexEdges current) distances

            current' = List.foldl (\x y ->
                case Map.lookup (vertexID x) distances' of
                    Nothing -> y
                    Just (xValue, _) -> case Map.lookup (vertexID y) distances' of
                        Nothing -> x
                        Just (yValue, _) -> if xValue < yValue then x else y)
                (head unvisited) unvisited

            unvisited' = List.delete current' unvisited
        in dijkstra' unvisited' current' distances'

    newDistances :: (Eq a, Ord a) => [Edge a] ->
        Map.Map a (Integer, Maybe (Vertex a)) ->
        Map.Map a (Integer, Maybe (Vertex a))
    newDistances [] distances = distances
    newDistances (edge:edges) distances =
        let currentDist = Number $ fst $ (distances) Map.! (vertexID $ start edge)
            tentativeDist = currentDist + (Number (cost edge))
            prevDist = case Map.lookup (vertexID (end edge)) distances of
                Nothing   -> PosInf
                Just (dist, prev) -> Number dist
            distances' = if prevDist == PosInf
                        then Map.insert (vertexID $ end edge)
                            (getValue tentativeDist, Just $ start edge) distances
                        else if tentativeDist < prevDist
                             then Map.update (\x -> Just
                                 (getValue tentativeDist, Just $ start edge))
                                 (vertexID $ end edge) distances
                             else distances
        in newDistances edges distances'

    getpath :: (Eq a, Ord a) => Vertex a -> Vertex a ->
        Map.Map a (Integer, Maybe (Vertex a)) -> Maybe (Path a)
    getpath start end distances = getpath' start end distances $ Just []

    getpath' :: (Eq a, Ord a) => Vertex a -> Vertex a ->
        Map.Map a (Integer, Maybe (Vertex a)) -> Maybe (Path a) ->
        Maybe (Path a)
    getpath' start end distances Nothing = Nothing
    getpath' start end distances (Just path) =
        if start == end then Just path else
            Map.lookup (vertexID end) distances >>=
            (\x -> Just $ snd x) >>=
            (\x -> case x of
                Nothing -> Nothing
                Just end' -> getpath' start end' distances $ Just (end : path))
