module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (delay)
import Data.Foldable (for_, intercalate)
import Data.Array (filter)
import React.Basic.DOM (render)
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM as R
import React.Basic.DOM.SVG as SVG
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useReducer, mkReducer, (/\), useState)
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks as React
import React.Basic.Events (handler_, handler)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.Window (document)
import Web.HTML.HTMLElement (toElement)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (fromMaybe)

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

backArrow :: JSX
backArrow =
  SVG.svg
    { className: "back-arrow"
    , viewBox: "0 0 24 24"
    , xmlns: "http://www.w3.org/2000/svg"
    , children:
        [ SVG.path
            { d: "M19 11H7.14l3.63-4.36a1 1 0 1 0-1.54-1.28l-5 6a1.19 1.19 0 0 0-.09.15c0 .05 0 .08-.07.13A1 1 0 0 0 4 12a1 1 0 0 0 .07.36c0 .05 0 .08.07.13a1.19 1.19 0 0 0 .09.15l5 6A1 1 0 0 0 10 19a1 1 0 0 0 .64-.23 1 1 0 0 0 .13-1.41L7.14 13H19a1 1 0 0 0 0-2z"
            }
        ]
    }

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

{--
                        , button [ class "form__submit" ] [ text "Submit" ]
                        ]
                    ]
                ]
            ]
        ]
      --}
mkApp :: Component {}
mkApp = do
  reducer <-
    mkReducer \state -> case _ of
      Increment -> state { count = state.count + 1 }
      Decrement -> state { count = state.count - 1 }
      ResetRequested -> state { resetting = true }
      Reset -> state { resetting = false, count = 1 }
  component "App" \_ -> React.do
    state /\ dispatch <- useReducer { count: 0, resetting: false } reducer
    useAff state.resetting do
      when state.resetting do
        liftEffect $ log $ show state.count
        delay $ Milliseconds 1000.0
        liftEffect $ dispatch Reset
    pure
      $ R.div_
          [ R.button
              { onClick: handler_ $ dispatch Increment
              , disabled: state.resetting
              , children: [ R.text "+" ]
              }
          , R.text $ show state.count
          , R.button
              { onClick: handler_ $ dispatch Decrement
              , disabled: state.resetting
              , children: [ R.text "-" ]
              }
          , R.br {}
          , R.button
              { onClick: handler_ $ dispatch ResetRequested
              , disabled: state.resetting
              , children: [ R.text "Reset" ]
              }
          ]
