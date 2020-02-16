module TopologicalSort
  ( getOrderedTypes
  )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Graph

import           API                            ( Name(..) )

buildDependencyGraph
  :: Set (Name, Maybe Name) -> (Graph, Vertex -> (Name, Name, [Name]))
buildDependencyGraph set =
  extractGraph $ graphFromEdges $ buildEdge <$> S.toList set
 where
  extractGraph (x, y, _z) = (transposeG x, y)
  buildEdge (n, Nothing     ) = (n, n, [])
  buildEdge (n, Just parentN) = (n, n, [parentN])

getOrderedTypes :: Set (Name, Maybe Name) -> [(Name, Maybe Name)]
getOrderedTypes set = do
  let (graph, nodeFromVertex) = buildDependencyGraph set
      vertexOrder             = topSort graph
  concat $ nodeToType . nodeFromVertex <$> vertexOrder
 where
  nodeToType :: (Name, Name, [Name]) -> [(Name, Maybe Name)]
  nodeToType (_, parentName, nodes) = case nodes of
    [] -> [(parentName, Nothing)]
    _  -> (\node -> (parentName, Just node)) <$> nodes
