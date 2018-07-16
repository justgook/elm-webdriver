module WebDriver.LowLevel.Value exposing (Value(..), Answer, decodeAnswer, answerDecoder)

{-| -}

import WebDriver.LowLevel.Value.Status as Status
import WebDriver.LowLevel.Value.Timeouts as Timeouts
import Json.Decode as Decode


{-| -}
type alias Answer =
    { status : Int
    , sessionId : String
    , value : Value
    }


type Value
    = NewSession Decode.Value
    | EndSession
    | Status Status.Value
    | GetTimeouts Timeouts.Value
    | SetTimeouts Timeouts.Value
    | Url
    | GetUrl String
    | Title String
    | WindowHandle WindowHandle
    | WindowHandles (List WindowHandle)
    | WindowHandleClose


type WindowHandle
    = Handle String



-- type alias Answer2 value =
--     { status : Int
--     , sessionId : String
--     , value : value
--     }
-- answerDecoder2 :
--     { sessionStart : Decode.Decoder (Answer2 Decode.Value)
--     }
-- answerDecoder2 =
--     { sessionStart = Decode.value |> decodeAnswer }


answerDecoder =
    { sessionStart = Decode.value |> Decode.map NewSession |> decodeAnswer
    , sessionStop = Decode.succeed EndSession |> decodeAnswer
    , status = Status.decode |> Decode.map Status |> decodeAnswer
    , getTimeouts = Timeouts.decode |> Decode.map GetTimeouts |> decodeAnswer
    , setTimeouts = Timeouts.decode |> Decode.map SetTimeouts |> decodeAnswer
    , url = Decode.succeed Url |> decodeAnswer
    , getUrl = Decode.string |> Decode.map GetUrl |> decodeAnswer
    , title = Decode.string |> Decode.map Title |> decodeAnswer
    , windowHandle = Decode.string |> Decode.map (Handle >> WindowHandle) |> decodeAnswer
    , windowHandles = Decode.string |> Decode.map Handle |> Decode.list |> Decode.map WindowHandles |> decodeAnswer
    , windowHandleClose = Decode.succeed WindowHandleClose |> decodeAnswer
    }



-- decodeAnswer : Decode.Decoder Value -> Decode.Decoder Answer


decodeAnswer : Decode.Decoder a -> Decode.Decoder { sessionId : String, status : number, value : a }
decodeAnswer decodeValue =
    let
        statusDecode =
            Decode.int

        sessionId =
            Decode.oneOf [ Decode.string, Decode.null "" ]
    in
        Decode.field "status" statusDecode
            |> Decode.andThen
                (\status ->
                    case status of
                        0 ->
                            Decode.map2
                                (\sessionId value ->
                                    { sessionId = sessionId
                                    , status = status
                                    , value = value
                                    }
                                )
                                (Decode.field "sessionId" sessionId)
                                (Decode.field "value" decodeValue)

                        _ ->
                            Decode.andThen Decode.fail decodeError
                )


decodeError : Decode.Decoder String
decodeError =
    Decode.at [ "value", "message" ] Decode.string
