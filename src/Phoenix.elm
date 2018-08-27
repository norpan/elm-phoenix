port module Phoenix exposing (Channel, Channels, Model, Msg, OutMsg(..), Response, init, listen, pushMsg, subscriptions, update)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Process
import Task
import Time



-- STATE --


{-| State model.

We have three states:

  - Closed - the initial state when nothing has happened yet.
  - Opening - the state where we have tried to open a web socket. Here we also keep track of:
      - url - the url we have tried to open, so that we can filter out old responses
      - attempts - the number of attempts at trying to open this url, used for exponential backoff
  - Open - the state where we have successfully opened a web socket. Here we also keep track of:
      - url - the url we have opened, so that we can filter out old responses
      - webSocket - the web socket, used for sending commands
      - channels - a dictionary of channels, used to keep track of channels, so that we know which ones to close

-}
type Model
    = Closed
    | Opening { url : String, attempts : Int }
    | Open
        { url : String
        , webSocket : Value
        , channels : Dict String { topic : String, params : Value, isJoined : Bool }
        , ref : Int
        }


{-| Channels to subscribe to.

Since channels are joined and left automatically by this package,
you need to provide a key for all channels. If that key is changed, the channel
is left and then rejoined. Useful if you want to set new parameters on the channel, for instance.

If you don't know which key to use, just use the channel topic.

-}
type alias Channels =
    Dict String Channel


{-| Channel model.

Keep track of channel topic, channel params.

-}
type alias Channel =
    { topic : String, params : Value }


{-| Internal messages. Takes a `msg` parameter because the `Push` method will contain an `onResponse` that generates "outer" messages.
-}
type Msg msg
    = OnOpen { target : Value }
    | OnError { target : Value }
    | OnChannelMessage Message
    | OnChannelResponse Response
    | RetryOpen { target : Value }
    | Heartbeat
    | Error JD.Error
    | Push { url : String, topic : String, event : String, payload : Value, onResponse : Response -> msg }


pushMsg : (Msg msg -> msg) -> { url : String, topic : String, event : String, payload : Value, onResponse : Response -> msg } -> msg
pushMsg onMsg =
    Push >> onMsg


type OutMsg
    = SocketRetry { url : String, wait : Float }
    | SocketConnected { url : String }
    | ChannelMessage Message



-- UPDATE --


{-| Update function
This is the regular model update function. Also returns an `OutMsg` for messages and responses from channels.
-}
update : (OutMsg -> msg) -> (Msg msg -> msg) -> Msg msg -> Model -> ( Model, Cmd msg )
update doOutMsg doMsg msg model =
    let
        doNothing =
            ( model, Cmd.none )

        setModel newModel =
            ( newModel, Cmd.none )

        setCmd cmd =
            ( model, cmd )

        setOutMsg outMsg =
            ( model, Task.perform doOutMsg (Task.succeed outMsg) )
    in
    case ( msg, model ) of
        -- Socket has been opened
        ( OnOpen { target }, Opening { url } ) ->
            if JD.decodeValue (JD.field "url" JD.string) target == Ok url then
                ( Open { url = url, webSocket = target, channels = Dict.empty, ref = 0 }
                , Task.perform doOutMsg (Task.succeed (SocketConnected { url = url }))
                )

            else
                -- Open event with the wrong url, close it
                setCmd (webSocketClose ( target, 1000, "New URL" ))

        -- Socket could not be opened, try again with exponential backoff
        ( OnError { target }, Opening { url, attempts } ) ->
            if JD.decodeValue (JD.field "url" JD.string) target == Ok url then
                let
                    -- Same backoff algorithm as in phoenix.js
                    wait =
                        case attempts of
                            0 ->
                                1000

                            1 ->
                                2000

                            2 ->
                                5000

                            _ ->
                                10000
                in
                ( Opening { url = url, attempts = attempts + 1 }
                , Cmd.batch
                    [ Process.sleep wait |> Task.perform (\_ -> doMsg (RetryOpen { target = target }))
                    , Task.perform doOutMsg (Task.succeed (SocketRetry { url = url, wait = wait }))
                    ]
                )

            else
                -- Error opening wrong url, ignore it
                doNothing

        ( RetryOpen { target }, Opening { url, attempts } ) ->
            if JD.decodeValue (JD.field "url" JD.string) target == Ok url then
                setCmd (webSocketOpen url)

            else
                -- Url has changed, don't retry old url
                doNothing

        ( OnChannelMessage message, _ ) ->
            setOutMsg (ChannelMessage message)

        {-
           ( OnChannelResponse response, Open ({ channels } as open) ) ->
               if response.topic == "phoenix" then
                   doNothing

               else
                   case Dict.get response.ref channels of
                       Nothing ->
                           setOutMsg (ChannelResponse response)

                       Just ({ topic, isJoined } as channel) ->
                           if topic == response.topic && not isJoined then
                               setModel
                                   (Open
                                       { open | channels = Dict.insert response.ref { channel | isJoined = True } channels }
                                   )

                           else
                               setOutMsg (ChannelResponse response)
        -}
        -- Push message
        ( Push { url, topic, event, payload, onResponse }, Open ({ webSocket, ref } as open) ) ->
            setCmd (sendRequest webSocket (push topic event payload ref))

        -- Channel subscribed
        ( Heartbeat, Open { webSocket } ) ->
            setCmd (sendRequest webSocket heartbeat)

        _ ->
            doNothing


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions onMsg model =
    case model of
        Closed ->
            webSocketOnError OnError
                |> Sub.map onMsg

        Opening _ ->
            Sub.batch
                [ webSocketOnOpen OnOpen
                , webSocketOnError OnError
                ]
                |> Sub.map onMsg

        Open _ ->
            Sub.batch
                [ Time.every 30000 (always Heartbeat)
                , webSocketOnMessage onMessage
                ]
                |> Sub.map onMsg


onMessage : { target : JD.Value, data : String } -> Msg msg
onMessage { data } =
    case JD.decodeString messageOrResponse data of
        Ok msg ->
            msg

        Err e ->
            Error e


messageOrResponse : Decoder (Msg msg)
messageOrResponse =
    JD.field "event" JD.string
        |> JD.andThen
            (\event ->
                case event of
                    "phx_reply" ->
                        JD.map OnChannelResponse responseDecoder

                    _ ->
                        JD.map OnChannelMessage messageDecoder
            )


statusDecoder : Decoder Status
statusDecoder =
    JD.string
        |> JD.map
            (\status ->
                case status of
                    "ok" ->
                        StatusOk

                    _ ->
                        StatusError
            )


listen : String -> Channels -> Model -> ( Model, Cmd msg )
listen newUrl newChannels model =
    case model of
        Closed ->
            ( Opening { url = newUrl, attempts = 0 }
            , webSocketOpen newUrl
            )

        Opening { url } ->
            if url /= newUrl then
                -- Reopen with new url
                ( Opening
                    { url = newUrl
                    , attempts = 0
                    }
                , webSocketOpen newUrl
                )

            else
                ( model, Cmd.none )

        Open { url, webSocket, channels, ref } ->
            if url /= newUrl then
                -- Reopen with new url
                ( Opening
                    { url = newUrl
                    , attempts = 0
                    }
                , Cmd.batch
                    [ webSocketClose ( webSocket, 1000, "New URL" )
                    , webSocketOpen newUrl
                    ]
                )

            else
                let
                    unchangedChannels =
                        Dict.filter (\key _ -> Dict.member key newChannels) channels

                    addedChannels =
                        Dict.filter (\key _ -> not (Dict.member key channels)) newChannels

                    openChannelsCmds =
                        addedChannels
                            |> Dict.toList
                            |> List.map
                                (\( key, { topic, params } ) ->
                                    sendRequest webSocket (openChannel key topic params)
                                )

                    removedChannels =
                        Dict.filter (\key _ -> not (Dict.member key newChannels)) channels

                    closeChannelsCmds =
                        removedChannels
                            |> Dict.values
                            |> List.map
                                (\{ topic } ->
                                    sendRequest webSocket (closeChannel topic)
                                )
                in
                ( Open
                    { url = url
                    , webSocket = webSocket
                    , channels =
                        Dict.union unchangedChannels
                            (Dict.map (\_ { topic, params } -> { topic = topic, params = params, isJoined = False }) addedChannels)
                    , ref = ref
                    }
                , Cmd.batch
                    (openChannelsCmds
                        ++ closeChannelsCmds
                    )
                )


closeChannel : String -> Request
closeChannel topic =
    { topic = topic
    , event = "phx_leave"
    , payload = JE.object []
    , ref = topic
    }


openChannel : String -> String -> Value -> Request
openChannel key topic params =
    { topic = topic
    , event = "phx_join"
    , payload = params
    , ref = key
    }


heartbeat : Request
heartbeat =
    { topic = "phoenix"
    , event = "heartbeat"
    , payload = JE.object []
    , ref = "heartbeat"
    }


push : String -> String -> Value -> Int -> Request
push topic event payload ref =
    { topic = topic
    , event = event
    , payload = payload
    , ref = String.fromInt ref
    }


type alias Request =
    { topic : String
    , event : String
    , payload : Value
    , ref : String
    }


type alias Message =
    { topic : String
    , event : String
    , payload : Value
    }


type alias Response =
    { topic : String
    , status : Status
    , response : Value
    , ref : String
    }


type Status
    = StatusOk
    | StatusError


sendRequest : Value -> Request -> Cmd msg
sendRequest webSocket request =
    webSocketSend ( webSocket, encodeRequest request )


encodeRequest : Request -> JE.Value
encodeRequest { topic, event, payload, ref } =
    JE.object
        [ ( "topic", JE.string topic )
        , ( "event", JE.string event )
        , ( "payload", payload )
        , ( "ref", JE.string ref )
        ]


messageDecoder : Decoder Message
messageDecoder =
    JD.map3 Message
        (JD.field "topic" JD.string)
        (JD.field "event" JD.string)
        (JD.field "payload" JD.value)


responseDecoder : Decoder Response
responseDecoder =
    JD.map4 Response
        (JD.field "topic" JD.string)
        (JD.at [ "payload", "status" ] statusDecoder)
        (JD.at [ "payload", "response" ] JD.value)
        (JD.field "ref" JD.string)


init : ( Model, Cmd (Msg msg) )
init =
    ( Closed
    , Task.perform identity (Task.succeed Heartbeat)
    )



-- COMMAND PORTS


port webSocketOpen : String -> Cmd msg


port webSocketSend : ( JD.Value, JD.Value ) -> Cmd msg


port webSocketClose : ( JD.Value, Int, String ) -> Cmd msg



-- SUBSCRIPTION PORTS


port webSocketOnOpen : ({ target : JD.Value } -> msg) -> Sub msg


port webSocketOnClose : (JD.Value -> msg) -> Sub msg


port webSocketOnError : ({ target : JD.Value } -> msg) -> Sub msg


port webSocketOnMessage : ({ target : JD.Value, data : String } -> msg) -> Sub msg
