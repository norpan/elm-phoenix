module Main exposing (main)

import Browser
import Dict
import Html exposing (Html)
import Html.Events
import Json.Encode as JE
import Phoenix


type alias Model =
    { phoenix : Phoenix.Model
    , phoenixUrl : String
    , phoenixChannels : Phoenix.Channels
    , messages : List Phoenix.OutMsg
    , response : Maybe Phoenix.Response
    }


type Msg
    = PhoenixMsg (Phoenix.Msg Msg)
    | PhoenixOutMsg Phoenix.OutMsg
    | ButtonClickResponse Phoenix.Response


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = phoenixUpdate
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( phoenixModel, phoenixCmd ) =
            Phoenix.init
    in
    ( { phoenix = phoenixModel
      , phoenixUrl = "ws://your-url/socket"
      , phoenixChannels = Dict.fromList [ ( "your_channel", { topic = "your_channel", params = JE.object [] } ) ]
      , messages = []
      , response = Nothing
      }
    , Cmd.map PhoenixMsg phoenixCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.subscriptions PhoenixMsg model.phoenix


phoenixUpdate : Msg -> Model -> ( Model, Cmd Msg )
phoenixUpdate msg model =
    let
        ( newModel, cmd ) =
            update msg model

        ( phoenixModel, phoenixCmd ) =
            Phoenix.listen model.phoenixUrl model.phoenixChannels newModel.phoenix
    in
    ( { newModel | phoenix = phoenixModel }, Cmd.batch [ cmd, phoenixCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg phoenixMsg ->
            let
                ( phoenixModel, phoenixCmd ) =
                    Phoenix.update PhoenixOutMsg PhoenixMsg phoenixMsg model.phoenix
            in
            ( { model | phoenix = phoenixModel }
            , phoenixCmd
            )

        PhoenixOutMsg outMsg ->
            ( { model | messages = outMsg :: model.messages }
            , Cmd.none
            )

        ButtonClickResponse response ->
            ( { model | response = Just response }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button
            [ Html.Events.onClick
                (Phoenix.pushMsg PhoenixMsg
                    { url = model.phoenixUrl
                    , topic = "your_channel"
                    , event = "your_event"
                    , payload = JE.object []
                    , onResponse = ButtonClickResponse
                    }
                )
            ]
            [ Html.text "Send push" ]
        , case model.response of
            Nothing ->
                Html.text ""

            Just response ->
                Html.dl []
                    [ Html.dt [] [ Html.text "ref" ]
                    , Html.dd [] [ Html.text response.ref ]
                    ]
        ]
