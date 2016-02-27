module Dijkstra ( Edge(..) , Vertex(..), Graph(..),
    Dijkstra.fromList, addVertex, dijkstra )
    where

import InfNum
import Data.List
import Data.Map

data Edge a = Edge { start :: Vertex a,
                     end   :: Vertex a,
                     cost  :: Integer }
    deriving (Show)

data Vertex a = Vertex { vertexID :: a,
                         vertexEdges :: [Edge a] }
    deriving (Show)

instance (Eq a) => Eq (Vertex a) where
    v1 == v2 = vertexID v1 == vertexID v2

data Graph a = Graph { graphVertices :: [Vertex a],
                         graphEdges    :: [Edge a] }
    deriving (Show)

{- Construct graphs. -}
fromList :: Eq a => [Vertex a] -> Graph a
fromList verteces = let edges = getEdges(verteces)
    in Graph verteces edges
    where getEdges [] = []
          getEdges (x:xs) = (vertexEdges x) ++ (getEdges verteces)

addVertex :: Graph a -> Vertex a -> Graph a
addVertex graph vertex =
    let vertexes = graphVertices graph
        vertexes' = vertex : vertexes
    in Graph vertexes' (graphEdges graph)

{- Find paths through a graph. -}
{-dijkstra :: (Eq a, Ord a) => a -> a -> Graph a -> [Edge a]-}
dijkstra :: (Eq a, Ord a) => a -> a -> Graph a -> Map a Integer
dijkstra vertex1 vertex2 graph =
    case find (\x -> (vertexID x) == vertex1) (graphVertices graph)
    of Nothing -> error "Can't find start edge in graph"
       Just start ->
           let distances = singleton (vertexID start) 0
               unvisited = Data.List.filter (\x -> not (vertex1 == vertexID x))
                   (graphVertices graph)
               distances' = dijkstra' unvisited start distances
           in distances'
    where
    dijkstra' :: (Eq a, Ord a) => [Vertex a] -> Vertex a -> (Map a Integer) ->
        (Map a Integer)
    dijkstra' [] current distances = distances
    dijkstra' unvisited current distances =
        let distances' = newDistances (vertexEdges current) distances

            current' = Data.List.foldl
                (\x y -> case Data.Map.lookup (vertexID x) distances'
                    of Nothing -> y
                       Just xValue -> case Data.Map.lookup (vertexID y) distances'
                        of Nothing -> x
                           Just yValue -> if xValue < yValue then x else y)
                (head unvisited) unvisited

            unvisited' = Data.List.delete current' unvisited
        in dijkstra' unvisited' current' distances'

    newDistances :: (Eq a, Ord a) => [Edge a] -> (Map a Integer) ->
        (Map a Integer)
    newDistances [] distances = distances
    newDistances (edge:edges) distances =
        let currentDist = Number $ distances Data.Map.! (vertexID $ start edge)
            tentativeDist = currentDist + (Number (cost edge))
            prevDist = case Data.Map.lookup (vertexID (end edge)) distances
                of Nothing   -> PosInf
                   Just dist -> Number dist
            distances' = if prevDist == PosInf
                        then Data.Map.insert (vertexID $ end edge)
                            (getValue tentativeDist) distances
                        else if tentativeDist < prevDist
                             then update (\x -> Just (getValue tentativeDist))
                                (vertexID $ end edge) distances
                             else distances
        in newDistances edges distances'
