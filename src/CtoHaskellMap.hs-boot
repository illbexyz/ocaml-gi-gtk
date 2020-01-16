module CtoHaskellMap ( cToHaskellMap, Hyperlink )
  where

import qualified Data.Map as M
import GtkDoc (CRef(..))
import API (API(..), Name(..))

data Hyperlink

cToHaskellMap :: [(Name, API)] -> M.Map CRef Hyperlink
