module TellMeT.Util where

import Data.Default (Default(def))
import Lens.Micro (ASetter, set)

make :: (Default a) => ASetter a a x b -> b -> a
make setter param = set setter param def
