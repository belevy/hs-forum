module Component.Page.Login where

import Prelude
import Component.BackArrow (mkBackArrow)
import Component.LoginForm (mkLoginForm)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component)
import Web.HTML (window)
import Web.HTML.History (back)
import Web.HTML.Window (history)

mkLogin :: Component {}
mkLogin = do
  backArrow <- mkBackArrow
  loginForm <- mkLoginForm
  component "Login" \_ -> do
    pure do
      R.div
        { className: "flex bg-light no-padding"
        , children:
            [ R.a
                { className: "icon-link"
                , children: [ backArrow ]
                , onClick:
                    capture_
                      $ window
                      >>= history
                      >>= back
                }
            , loginForm {}
            ]
        }
