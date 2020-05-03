module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Events
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Http
import Json.Encode as Encode
import Url exposing (Url)


type alias Model =
    { username : String
    , password : String
    , navKey : Browser.Navigation.Key
    , device : Device
    }


initialModel flags navKey =
    { username = ""
    , password = ""
    , navKey = navKey
    , device = classifyDevice flags.window
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
    | BrowserResized Int Int


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

        BrowserResized width height ->
            ( { model | device = classifyDevice { width = width, height = height } }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Title"
    , body =
        [ Element.layout [ width fill, height fill ] <|
            el
                [ width (px 320)
                , padding 32
                , Border.shadow
                    { offset = ( 2, 2 )
                    , size = 3
                    , blur = 3
                    , color = rgba 0 0 0 0.3
                    }
                , centerX
                , centerY
                ]
            <|
                column [ width fill, spacing 16 ]
                    [ Input.username [ width fill ]
                        { onChange = UsernameUpdated
                        , text = model.username
                        , placeholder = Nothing
                        , label = Input.labelAbove [] <| text "Username"
                        }
                    , Input.currentPassword [ width fill ]
                        { onChange = PasswordUpdated
                        , text = model.password
                        , placeholder = Nothing
                        , label = Input.labelAbove [] <| text "Password"
                        , show = False
                        }
                    , Input.button [ width fill, Background.color (rgb255 0x33 0x88 0xCD), padding 12 ]
                        { onPress = Just FormSubmitted
                        , label = el [ centerX ] <| text "Login"
                        }
                    ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize BrowserResized
