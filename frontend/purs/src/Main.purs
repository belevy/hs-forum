module Main where

import Prelude
import Effect (Effect)
import Data.Foldable (for_, intercalate)
import Data.Array (filter)
import React.Basic.DOM (render)
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, (/\), useState)
import React.Basic.Hooks as React
import React.Basic.Events (handler)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.Window (document)
import Web.HTML.HTMLElement (toElement)
import Data.Maybe (fromMaybe)
import Components.BackArrow (mkBackArrow)

main :: Effect Unit
main = do
  b <- body =<< document =<< window
  for_ b \el -> do
    form <- mkForm
    render (form {}) (toElement el)

data Action
  = Increment
  | Decrement
  | ResetRequested
  | Reset

classNames :: Array { className :: String, active :: Boolean } -> String
classNames =
  intercalate " "
    <<< map (_.className)
    <<< filter (_.active)

mkFieldSet ::
  Component
    { name :: String
    , label :: String
    , isActive :: Boolean
    , value :: String
    , setValue :: (String -> String) -> Effect Unit
    }
mkFieldSet = do
  component "FieldSet"
    $ \{ name, label, isActive, value, setValue } -> React.do
        pure
          $ R.fieldset
              { className:
                  classNames
                    [ { className: "form__fieldset", active: true }
                    , { className: "form__fieldset--active", active: isActive }
                    ]
              , children:
                  [ R.label
                      { htmlFor: name
                      , className: "form__fieldset__label"
                      , children: [ R.text label ]
                      }
                  , R.input
                      { name: name
                      , className: "form__fieldset__input"
                      , value: value
                      , onChange:
                          handler targetValue \v ->
                            setValue \oldValue ->
                              fromMaybe oldValue v
                      }
                  ]
              }

mkForm :: Component {}
mkForm = do
  backArrow <- mkBackArrow
  fieldSet <- mkFieldSet
  component "LoginForm"
    $ \_ -> React.do
        username /\ setUsername <- useState ""
        password /\ setPassword <- useState ""
        pure
          $ R.div
              { className: "flex bg-light no-padding"
              , children:
                  [ R.a
                      { className: "icon-link"
                      , children: [ backArrow ]
                      }
                  , R.div
                      { className: "centered card card--sm"
                      , children:
                          [ R.div { className: "card__header", children: [ R.text "Login" ] }
                          , R.div
                              { className: "card__body"
                              , children:
                                  [ R.form
                                      { className: "form flex--vertical"
                                      , children:
                                          [ fieldSet
                                              { isActive: username /= ""
                                              , label: "Username"
                                              , name: "username"
                                              , value: username
                                              , setValue: setUsername
                                              }
                                          , fieldSet
                                              { isActive: password /= ""
                                              , label: "Password"
                                              , name: "password"
                                              , value: password
                                              , setValue: setPassword
                                              }
                                          , R.button
                                              { className: "form__submit"
                                              , children: [ R.text "Submit" ]
                                              }
                                          ]
                                      }
                                  ]
                              }
                          ]
                      }
                  ]
              }
