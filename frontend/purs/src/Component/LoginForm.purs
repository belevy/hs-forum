module Component.LoginForm
  ( mkLoginForm
  ) where

import Prelude
import Affjax (defaultRequest)
import Affjax as AX
import Affjax.RequestBody (json) as RequestBody
import Component.BackArrow (mkBackArrow)
import Component.Form.Fieldset (mkFieldSet)
import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.Either as Either
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

data SubmissionState
  = NotSubmitting
  | SubmissionInProgress
  | SubmissionSucceeded
  | SubmissionFailed

derive instance submissionStateEq :: Eq SubmissionState

instance submissionStateShow :: Show SubmissionState where
  show NotSubmitting = "NotSubmitting"
  show SubmissionInProgress = "SubmissionInProgress"
  show SubmissionSucceeded = "SubmissionSucceeded"
  show SubmissionFailed = "SubmissionFailed"

mkLoginForm :: Component {}
mkLoginForm = do
  backArrow <- mkBackArrow
  fieldSet <- mkFieldSet
  component "LoginForm"
    $ \_ -> React.do
        username /\ setUsername <- useState ""
        password /\ setPassword <- useState ""
        submitting /\ setSubmitting <- useState NotSubmitting
        useAff submitting do
          when (submitting == (SubmissionInProgress)) do
            response <-
              AX.request do
                defaultRequest
                  { url = "/api/sessions"
                  , method = Left POST
                  , withCredentials = true
                  , content =
                    Just $ RequestBody.json $ encodeJson
                      $ { user_name: username, password: password }
                  }
            if Either.isRight response then
              liftEffect $ setSubmitting $ const SubmissionSucceeded
            else
              liftEffect $ setSubmitting $ const SubmissionFailed
        pure do
          R.div
            { className: "centered card card--sm"
            , children:
                [ R.div { className: "card__header", children: [ R.text "Login" ] }
                , R.div
                    { className: "card__body"
                    , children:
                        [ R.h1_ [ R.text $ show submitting ]
                        , R.form
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
                                    , disabled: submitting == SubmissionInProgress
                                    , onClick:
                                        capture_ do
                                          setSubmitting (const $ SubmissionInProgress)
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
