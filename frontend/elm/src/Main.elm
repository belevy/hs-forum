module Main exposing (..)

import Browser exposing (UrlRequest)
import Html exposing (text)
import Url exposing (Url)


type alias Model =
    ()


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url


main : Program () Model Msg
main =
    Browser.application
        { init = \() url_ key_ -> ( (), Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


update msg_ model =
    ( model, Cmd.none )


view model_ =
    { title = "Title"
    , body = [ text "goodbye" ]
    }
