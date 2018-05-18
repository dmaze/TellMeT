{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.Counter where

import Data.Default (Default)
import Data.Maybe (maybe)
import Lens.Micro ((^.), (^?), Lens', Traversal')
import Lens.Micro.Mtl ((+=), (-=))
import Miso ( View
            , input_, class_, readonly_, type_, value_, onClick, text 
#ifdef __GHCJS__
            , Transition
#endif
            )
import Miso.String (ms)

import TellMeT.Bootstrap (btn_, input_group_)
import TellMeT.Util (make)

class CounterModel a where
  counterValue :: Lens' a Int

class CounterAction a where
  inc :: Traversal' a ()
  dec :: Traversal' a ()

viewCounterValue :: (CounterModel model) => model -> View action
viewCounterValue m =
  input_ [ type_ "number", readonly_ True, class_ "form-control-plaintext",
           value_ (ms $ show $ m ^. counterValue)]

viewPlus :: (CounterAction action, Default action) => model -> View action
viewPlus _ = btn_ [ onClick $ make inc () ] [ text "+" ]

viewMinus :: (CounterAction action, Default action) => model -> View action
viewMinus _ = btn_ [ onClick $ make dec () ] [ text "-" ]

viewCounter :: (CounterModel model, CounterAction action, Default action)
            => model -> View action
viewCounter m = input_group_ [viewMinus m] [viewCounterValue m] [viewPlus m]

#ifdef __GHCJS__
updateCounter :: (CounterModel model, CounterAction action)
              => action -> Transition action model ()
updateCounter action = do
  maybe (return ()) (\() -> counterValue += 1) (action ^? inc)
  maybe (return ()) (\() -> counterValue -= 1) (action ^? dec)
#endif
