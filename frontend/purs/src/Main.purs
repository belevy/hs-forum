module Main where

import Prelude 
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (delay)
import Data.Foldable (for_)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useReducer, mkReducer, (/\))
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks as React
import React.Basic.Events (handler_)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.Window (document)
import Web.HTML.HTMLElement (toElement)
import Data.Time.Duration (Milliseconds(..))

main :: Effect Unit
main = do
  b <- body =<< document =<< window
  for_ b \el -> do
    app <- mkApp
    render (app {}) (toElement el)

data Action 
  = Increment
  | Decrement
  | ResetRequested
  | Reset
  
mkApp :: Component {}
mkApp = do
  reducer <- mkReducer \state -> case _ of
        Increment -> 
          state { count = state.count + 1}
        Decrement -> 
          state { count = state.count - 1}
        ResetRequested -> 
          state { resetting = true }
        Reset ->
          state { resetting = false, count = 1 }
  component "App" \_ -> React.do 
    state /\ dispatch <- useReducer {count: 0, resetting: false} reducer
    useAff state.resetting do 
       when state.resetting do
         liftEffect $ log $ show state.count
         delay $ Milliseconds 1000.0
         liftEffect $ dispatch Reset
    pure $ R.div_ 
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
