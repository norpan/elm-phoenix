module Main exposing (main)

import Browser
import Dict
import Html exposing (Html)
import Json.Encode as JE
import Phoenix


type alias Model =
    { phoenix : Phoenix.Model
    , phoenixUrl : String
    , phoenixChannels : Phoenix.Channels
    , messages : List Phoenix.OutMsg
    }


type Msg
    = PhoenixMsg Phoenix.Msg


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
      }
    , Cmd.map PhoenixMsg phoenixCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PhoenixMsg (Phoenix.subscriptions model.phoenix)


phoenixUpdate : Msg -> Model -> ( Model, Cmd Msg )
phoenixUpdate msg model =
    let
        ( newModel, cmd ) =
            update msg model

        ( phoenixModel, phoenixCmd ) =
            Phoenix.listen model.phoenixUrl model.phoenixChannels newModel.phoenix
    in
    ( { newModel | phoenix = phoenixModel }, Cmd.batch [ cmd, phoenixCmd ] )


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton >> Maybe.withDefault []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg phoenixMsg ->
            let
                ( phoenixModel, phoenixCmd, outMsg ) =
                    Phoenix.update phoenixMsg model.phoenix
            in
            ( { model
                | phoenix = phoenixModel
                , messages = model.messages ++ maybeToList outMsg
              }
            , Cmd.map PhoenixMsg phoenixCmd
            )


view : Model -> Html Msg
view model =
    Html.text ""
