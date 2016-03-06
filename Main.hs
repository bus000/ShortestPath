{- |
 - Module      :  Main
 - Description :  Test the Dijkstra module.
 - Copyright   :  (c) Magnus Stavngaard
 - License     :  <license>
 -
 - Maintainer  :  magnus@stavngaard.dk
 - Stability   :  experimental
 - Portability :  portable
 -
 -}
module Main
    where

import Dijkstra
import InfNum

-- | Test the implementation of the Dijkstra algorithm.
main :: IO ()
main = do
    let vertex0 = Vertex { vertexID = 0, vertexEdges = [] }
        vertex1 = Vertex { vertexID = 1, vertexEdges = [] }
        vertex2 = Vertex { vertexID = 2, vertexEdges = [] }
        vertex3 = Vertex { vertexID = 3, vertexEdges = [] }
        vertex4 = Vertex { vertexID = 4, vertexEdges = [] }
        vertex5 = Vertex { vertexID = 5, vertexEdges = [] }

        edge0  = Edge { start = vertex0, end = vertex1, cost = 5 }
        edge1  = Edge { start = vertex0, end = vertex3, cost = 1 }
        edge2  = Edge { start = vertex1, end = vertex2, cost = 2 }
        edge3  = Edge { start = vertex1, end = vertex3, cost = 7 }
        edge4  = Edge { start = vertex2, end = vertex1, cost = 4 }
        edge5  = Edge { start = vertex2, end = vertex5, cost = 4 }
        edge6  = Edge { start = vertex3, end = vertex2, cost = 9 }
        edge7  = Edge { start = vertex3, end = vertex4, cost = 12 }
        edge8  = Edge { start = vertex4, end = vertex2, cost = 3 }
        edge9  = Edge { start = vertex4, end = vertex5, cost = 2 }

        vertex0' = vertex0 { vertexEdges = [edge0, edge1] }
        vertex1' = vertex1 { vertexEdges = [edge2, edge3] }
        vertex2' = vertex2 { vertexEdges = [edge4, edge5] }
        vertex3' = vertex3 { vertexEdges = [edge6, edge7] }
        vertex4' = vertex4 { vertexEdges = [edge8, edge9] }
        vertex5' = vertex5

        edges = [edge0, edge1, edge2, edge3, edge4, edge5, edge6, edge7, edge8,
            edge9]
        vertices = [vertex0', vertex1', vertex2', vertex3', vertex4', vertex5']

        graph = Graph vertices edges
        graph' = dijkstra 0 5 graph
    print "Distances"

    case graph' of
        Nothing -> putStrLn "Nothing"
        Just path -> putStrLn $ showPath path
