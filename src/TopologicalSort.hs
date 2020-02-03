module TopologicalSort
  ( getOrderedTypes
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Graph

import           API                            ( Name(..) )
import           Naming                         ( ocamlIdentifier
                                                , nsOCamlIdentifier
                                                )

buildDependencyGraph
  :: Set (Name, Maybe Name) -> (Graph, Vertex -> (Text, Text, [Text]))
buildDependencyGraph set =
  extractGraph $ graphFromEdges $ buildEdge <$> S.toList set
 where
  extractGraph (x, y, z) = (transposeG x, y)
  buildEdge (n, Nothing) = (ocamlIdentifier n, ocamlIdentifier n, [])
  buildEdge (n, (Just parentName@(Name "GObject" "Object"))) =
    (ocamlIdentifier n, ocamlIdentifier n, [ocamlIdentifier parentName])
  buildEdge (n@(Name ns1 _), (Just parentName@(Name ns2 _))) | ns1 == ns2 =
    (ocamlIdentifier n, ocamlIdentifier n, [ocamlIdentifier parentName])
  buildEdge (n@(Name ns1 _), (Just parentName)) =
    (ocamlIdentifier n, ocamlIdentifier n, [nsOCamlIdentifier ns1 parentName])

getOrderedTypes :: Set (Name, Maybe Name) -> [(Text, Maybe Text)]
getOrderedTypes set = do
  let (graph, nodeFromVertex) = buildDependencyGraph set
      vertexOrder             = topSort graph
  concat $ nodeToType <$> nodeFromVertex <$> vertexOrder
 where
  nodeToType :: (Text, Text, [Text]) -> [(Text, Maybe Text)]
  nodeToType (_, parentName, nodes) = case nodes of
    [] -> [(parentName, Nothing)]
    _  -> (\node -> (parentName, Just node)) <$> nodes
