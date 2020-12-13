module Component.Form.Fieldset
  ( mkFieldSet
  ) where

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Html (classList)
import Prelude (Unit, pure, ($))
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component)

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
                  classList
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
