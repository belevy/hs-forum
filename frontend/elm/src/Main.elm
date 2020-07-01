module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , for
        , name
        , type_
        , value
        )
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Svg exposing (g, path, svg)
import Svg.Attributes as Svg exposing (d, viewBox)
import Url exposing (Url)


type alias Model =
    { username : String
    , password : String
    , navKey : Navigation.Key
    , session : Maybe String
    }


initialModel : Flags -> Navigation.Key -> Model
initialModel flags navKey =
    { username = ""
    , password = ""
    , navKey = navKey
    , session = Nothing
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
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
    | NavigateBack


type alias Flags =
    {}


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
            ( { model | username = "", password = "", session = Just "a session" }, Cmd.none )

        NavigateBack ->
            ( model, Navigation.back model.navKey 1 )


viewBackArrow =
    svg [ Svg.class "back-arrow", viewBox "0 0 24 24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path
            [ d "M19 11H7.14l3.63-4.36a1 1 0 1 0-1.54-1.28l-5 6a1.19 1.19 0 0 0-.09.15c0 .05 0 .08-.07.13A1 1 0 0 0 4 12a1 1 0 0 0 .07.36c0 .05 0 .08.07.13a1.19 1.19 0 0 0 .09.15l5 6A1 1 0 0 0 10 19a1 1 0 0 0 .64-.23 1 1 0 0 0 .13-1.41L7.14 13H19a1 1 0 0 0 0-2z" ]
            []
        ]


view : Model -> Document Msg
view model =
    { title = "Title"
    , body =
        [ div [ class "flex bg-light no-padding" ]
            [ a [ class "icon-link", onClick NavigateBack ] [ viewBackArrow ]
            , div [ class "centered card card--sm" ]
                [ div [ class "card__header" ]
                    [ text "Login" ]
                , div [ class "card__body" ]
                    [ form [ class "form flex--vertical", onSubmit FormSubmitted ]
                        [ fieldset
                            [ classList
                                [ ( "form__fieldset", True )
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
