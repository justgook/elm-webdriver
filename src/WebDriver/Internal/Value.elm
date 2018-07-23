module WebDriver.Internal.Value
    exposing
        ( Answer
        , AnswerDecoder
        , Element(..)
        , Out
        , Selector(..)
        , WindowHandle(..)
        , answerDecoder
        , decodeAnswer
        , jsonFromSelector
        )

{-| -}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import Util.Tuple as Tuple
import WebDriver.Internal.Value.Status as Status exposing (Status)
import WebDriver.Internal.Value.Timeouts as Timeouts exposing (Timeouts)


type alias Out value =
    Http.Request (Answer value)


{-| -}
type WindowHandle
    = Handle String


type alias Answer value =
    { status : Int
    , sessionId : String
    , value : value
    }


type alias AnswerDecoder value =
    Decode.Decoder (Answer value)


type Selector
    = CSSselector String
    | LinkText String
    | PartialLinkText String
    | TagName String
    | XPat String


type Element
    = Element String


jsonFromSelector : Selector -> Json.Value
jsonFromSelector selector =
    (case selector of
        CSSselector q ->
            ( "css selector", q )

        LinkText q ->
            ( "link text", q )

        PartialLinkText q ->
            ( "partial link text", q )

        TagName q ->
            ( "tag name", q )

        XPat q ->
            ( "xpath", q )
    )
        |> Tuple.mapBoth
            (\using -> ( "using", Json.string using ))
            (\value -> ( "value", Json.string value ))
        |> (\( a, b ) -> [ a, b ])
        |> Json.object


answerDecoder :
    { getTimeouts : AnswerDecoder Timeouts
    , element : AnswerDecoder Element
    , bool : AnswerDecoder Bool
    , elements : AnswerDecoder (List Element)
    , value : AnswerDecoder Decode.Value
    , setTimeouts : AnswerDecoder Timeouts
    , status : AnswerDecoder Status
    , string : AnswerDecoder String
    , listSring : AnswerDecoder (List String)
    , empty : AnswerDecoder ()
    , windowHandle : AnswerDecoder WindowHandle
    , windowHandles : AnswerDecoder (List WindowHandle)
    , decodeRect : AnswerDecoder { height : Int, width : Int, x : Int, y : Int }
    }
answerDecoder =
    { value = Decode.value |> decodeAnswer
    , status = Status.decode |> decodeAnswer
    , element = decodeElement |> Decode.map Element |> decodeAnswer
    , elements = decodeElement |> Decode.map Element |> Decode.list |> decodeAnswer
    , getTimeouts = Timeouts.decode |> decodeAnswer
    , setTimeouts = Timeouts.decode |> decodeAnswer
    , empty = Decode.null () |> decodeAnswer
    , string = Decode.string |> decodeAnswer
    , listSring = Decode.string |> Decode.list |> decodeAnswer
    , bool = Decode.bool |> decodeAnswer
    , windowHandle = Decode.string |> Decode.map Handle |> decodeAnswer
    , windowHandles = Decode.string |> Decode.map Handle |> Decode.list |> decodeAnswer
    , decodeRect = decodeRect |> decodeAnswer
    }


rect : Int -> Int -> Int -> Int -> { height : Int, width : Int, x : Int, y : Int }
rect a b c d =
    { height = a
    , width = b
    , x = c
    , y = d
    }


decodeRect : Decoder { height : Int, width : Int, x : Int, y : Int }
decodeRect =
    Decode.map4 rect
        (Decode.field "height" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


decodeElement : Decoder String
decodeElement =
    Decode.field "ELEMENT" Decode.string


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
