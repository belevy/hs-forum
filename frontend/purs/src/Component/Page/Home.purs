module Component.Page.Home where

import Prelude
import React.Basic.DOM (h1_, text)
import React.Basic.Hooks (Component, component)

mkHome :: Component {}
mkHome =
  component "Home" \_ -> do
    pure do
      h1_ [ text "Home" ]
