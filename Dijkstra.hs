module Dijkstra ( Edge(..) , Vertex(..), Graph(..),
    fromList, addVertex )
    where

import InfNum
import Data.List

data Edge a b = Edge { start :: Vertex a b,
                       end   :: Vertex a b,
                       cost  :: Integer }
    deriving (Show)

data Vertex a b = Vertex { vertexID :: a,
                           vertexEdges :: [Edge a b],
                           vertexValue :: InfNum b }
    deriving (Show)

instance (Eq a) => Eq (Vertex a b) where
    v1 == v2 = vertexID v1 == vertexID v2

data Graph a b = Graph { graphVertices :: [Vertex a b],
                         graphEdges    :: [Edge a b] }
    deriving (Show)

{- Construct graphs. -}
fromList :: Eq a => [Vertex a b] -> Graph a b
fromList verteces = let edges = getEdges(verteces)
    in Graph verteces edges
    where getEdges [] = []
          getEdges (x:xs) = (vertexEdges x) ++ (getEdges verteces)

addVertex :: Graph a b -> Vertex a b -> Graph a b
addVertex graph vertex =
    let vertexes = graphVertices graph
        vertexes' = vertex : vertexes
    in Graph vertexes' (graphEdges graph)

{- Find paths through graph. -}
dijkstra :: (Eq a, Num b) => a -> a -> Graph a b -> [Edge a b]
dijkstra vertex1 vertex2 graph =
    case find (\x -> (vertexID x) == vertex1) (graphVertices graph)
        of Nothing -> error "Can't find start edge"
           Just start ->
            let graph' = graph { graphVertices = dijkstraInit start
                (graphVertices graph) }
            in graphEdges graph'

    where
        dijkstraInit vertex1 =
            map (\x -> if x == vertex1
                       then x { vertexValue = Number 1 }
                       else x { vertexValue = PosInf } )
