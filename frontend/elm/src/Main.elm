module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation
import Html exposing (..)
import Html.Attributes
    exposing
        ( class
        , classList
        , for
        , name
        , type_
        , value
        )
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Url exposing (Url)


type alias Model =
    { username : String
    , password : String
    , navKey : Browser.Navigation.Key
    }


initialModel flags navKey =
    { username = ""
    , password = ""
    , navKey = navKey
    }


init flags url navKey =
    ( initialModel flags navKey
    , Cmd.none
    )


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | UsernameUpdated String
    | PasswordUpdated String
    | FormSubmitted
    | SessionCreateResponse (Result Http.Error ())


type alias WindowObject =
    { height : Int
    , width : Int
    }


type alias Flags =
    { window : WindowObject
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias UserCredentials =
    { username : String
    , password : String
    }


encodeCredentials : UserCredentials -> Encode.Value
encodeCredentials c =
    Encode.object
        [ ( "user_name", Encode.string c.username )
        , ( "password", Encode.string c.password )
        ]


createSession : (Result Http.Error () -> msg) -> UserCredentials -> Cmd msg
createSession toMsg creds =
    Http.post
        { body = Http.jsonBody <| encodeCredentials creds
        , expect = Http.expectWhatever toMsg
        , url = "/api/sessions"
        }


update msg model =
    case msg of
        UrlRequested request_ ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        UsernameUpdated newUsername ->
            ( { model | username = newUsername }
            , Cmd.none
            )

        PasswordUpdated newPassword ->
            ( { model | password = newPassword }
            , Cmd.none
            )

        FormSubmitted ->
            let
                submitCmd =
                    if model.password /= "" && model.username /= "" then
                        createSession SessionCreateResponse { username = model.username, password = model.password }

                    else
                        Cmd.none
            in
            ( model
            , submitCmd
            )

        SessionCreateResponse res ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Title"
    , body =
        [ div [ class "flex" ]
            [ div [ class "centered card" ]
                [ div [ class "card__header" ]
                    [ text "Login" ]
                , div [ class "card__body" ]
                    [ form [ class "form flex--vertical" ]
                        [ fieldset
                            [ classList
                                [ ( "form__fieldset", True )
                                , ( "form__fieldset--label-above", True )
                                , ( "form__fieldset--active", model.username /= "" )
                                ]
                            ]
                            [ label
                                [ for "username"
                                , class "form__fieldset__label"
                                ]
                                [ text "Username" ]
                            , input
                                [ name "username"
                                , class "form__fieldset__input"
                                , onInput UsernameUpdated
                                , value model.username
                                ]
                                []
                            ]
                        , fieldset
                            [ classList
                                [ ( "form__fieldset", True )
                                , ( "form__fieldset--label-above", True )
                                , ( "form__fieldset--active", model.password /= "" )
                                ]
                            ]
                            [ label
                                [ for "password"
                                , class "form__fieldset__label"
                                ]
                                [ text "Password" ]
                            , input
                                [ name "password"
                                , type_ "password"
                                , class "form__fieldset__input"
                                , onInput PasswordUpdated
                                , value model.password
                                ]
                                []
                            ]
                        , button [ class "form__submit" ] [ text "Submit" ]
                        ]
                    ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
