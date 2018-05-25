{-# LANGUAGE OverloadedStrings #-}

-- | Page-level wrapper.
module TellMeT.Components.Chrome where

import Miso.Html (View, text)
import Miso.Html.Element (div_)
import Network.URI (URI)

import TellMeT.Bootstrap (col_sm_, container_, navbar_, navbar_brand_, row_)

viewChrome :: URI -> View action -> View action
viewChrome home inner = div_ []
  [ navbar_
    [ navbar_brand_ home [text "TellMeT"] ]
  , container_
    [ row_
      [ col_sm_
        [ inner
        ]
      ]
    ]
  ]
