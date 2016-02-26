module Main
    where

import Dijkstra
import InfNum

main :: IO ()
main = do
    let vertex1 = Vertex { vertexID = 1, vertexEdges = [], vertexValue = 123 }
        vertex2 = Vertex { vertexID = 2, vertexEdges = [], vertexValue = 123 }
        vertex3 = Vertex { vertexID = 2, vertexEdges = [], vertexValue = 123 }
        vertex4 = Vertex { vertexID = 2, vertexEdges = [], vertexValue = 123 }
        vertex5 = Vertex { vertexID = 2, vertexEdges = [], vertexValue = 123 }
        vertex6 = Vertex { vertexID = 2, vertexEdges = [], vertexValue = 123 }
        vertices = [vertex1, vertex2, vertex3, vertex4, vertex5, vertex6]
    print vertices
