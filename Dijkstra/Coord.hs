module Dijkstra.Coord where

import Linear.V2
import Control.DeepSeq     (NFData(..))

type Coord = V2 Int

instance NFData a => NFData (V2 a) where rnf (V2 a b) = rnf a `seq` rnf b `seq` ()
