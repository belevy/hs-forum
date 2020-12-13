module Main where

import Prelude
import Component.LoginForm (mkLoginForm)
import Data.Foldable (for_)
import Effect (Effect)
import React.Basic.DOM (render)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  b <- body =<< document =<< window
  let a = 1
  for_ b \el -> do
    form <- mkLoginForm
    render (form {}) (toElement el)
