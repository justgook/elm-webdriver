module WebDriver.Internal.Value exposing
    ( Answer
    , AnswerDecoder
    , Cookie
    , Element(..)
    , Out
    , Selector(..)
    , WindowHandle(..)
    , WithSession
    , answerDecoder
    , decodeAnswer
    , decodeAnswerWithSession
    , jsonFromSelector
    )

{-| -}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import WebDriver.Internal.Value.Status as Status exposing (Status)
import WebDriver.Internal.Value.Timeouts as Timeouts exposing (Timeouts)


type alias WithSession =
    { sessionId : String, url : String }


type alias Out value =
    Http.Request (Answer value)


{-| -}
type WindowHandle
    = Handle String


type alias Answer value =
    { value : value
    }


type alias AnswerWithSession value =
    { sessionId : String
    , value : value
    }


type alias AnswerDecoder value =
    Decoder (Answer value)


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
    , decodeRect : AnswerDecoder { height : Float, width : Float, x : Float, y : Float }
    , cookie : AnswerDecoder Cookie
    , cookies : AnswerDecoder (List Cookie)
    }
answerDecoder =
    { value = Decode.value |> decodeAnswer
    , status = Status.decode |> decodeAnswer
    , element = decodeElement |> Decode.map Element |> decodeAnswer
    , elements = decodeElement |> Decode.map Element |> Decode.list |> decodeAnswer
    , getTimeouts = Timeouts.decode |> decodeAnswer
    , setTimeouts = Timeouts.decode |> decodeAnswer
    , empty = Decode.oneOf [ Decode.null (), decodeEmptyObject ] |> decodeAnswer
    , string = Decode.string |> decodeAnswer
    , listSring = Decode.string |> Decode.list |> decodeAnswer
    , bool = Decode.bool |> decodeAnswer
    , windowHandle = Decode.string |> Decode.map Handle |> decodeAnswer
    , windowHandles = Decode.string |> Decode.map Handle |> Decode.list |> decodeAnswer
    , decodeRect = decodeRect |> decodeAnswer
    , cookie = decodeCookie |> decodeAnswer
    , cookies = decodeCookies |> decodeAnswer
    }


type alias Cookie =
    { name : String
    , value : String
    , domain : Maybe String
    , expiry : Maybe Int
    , httpOnly : Maybe Bool
    , path : Maybe String
    , secure : Maybe Bool
    }


decodeEmptyObject : Decoder ()
decodeEmptyObject =
    Decode.keyValuePairs (Decode.null ())
        |> Decode.andThen
            (List.head
                >> Maybe.map (\_ -> Decode.fail "Expected to to get empty Object")
                >> Maybe.withDefault (Decode.succeed ())
            )


decodeCookies : Decoder (List Cookie)
decodeCookies =
    Decode.list decodeCookie


decodeCookie : Decoder Cookie
decodeCookie =
    Decode.map7 Cookie
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.string)
        (Decode.maybe (Decode.field "domain" Decode.string))
        (Decode.maybe (Decode.field "expiry" Decode.int))
        (Decode.maybe (Decode.field "httpOnly" Decode.bool))
        (Decode.maybe (Decode.field "path" Decode.string))
        (Decode.maybe (Decode.field "secure" Decode.bool))


rect : Float -> Float -> Float -> Float -> { height : Float, width : Float, x : Float, y : Float }
rect a b c d =
    { height = a
    , width = b
    , x = c
    , y = d
    }


decodeRect : Decoder { height : Float, width : Float, x : Float, y : Float }
decodeRect =
    Decode.map4 rect
        (Decode.field "height" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


decodeElement : Decoder String
decodeElement =
    Decode.keyValuePairs Decode.string
        |> Decode.andThen
            (List.head
                >> Maybe.map (Tuple.second >> Decode.succeed)
                >> Maybe.withDefault (Decode.fail "Cannot get value for element")
            )


decodeAnswerWithSession : Decode.Decoder value -> Decoder (AnswerWithSession value)
decodeAnswerWithSession decodeValue =
    let
        statusDecode =
            Decode.int

        sessionId =
            Decode.oneOf [ Decode.string, Decode.null "" ]

        chrome =
            Decode.field "status" statusDecode
                |> Decode.andThen
                    (\status ->
                        case status of
                            0 ->
                                Decode.map2
                                    (\sessionId_ value ->
                                        { sessionId = sessionId_
                                        , value = value
                                        }
                                    )
                                    (Decode.field "sessionId" sessionId)
                                    (Decode.field "value" decodeValue)

                            _ ->
                                Decode.andThen Decode.fail decodeError
                    )

        firefox =
            Decode.map2
                (\sessionId_ value ->
                    { sessionId = sessionId_
                    , value = value
                    }
                )
                (Decode.at [ "value", "sessionId" ] sessionId)
                (Decode.oneOf
                    [ Decode.at [ "value", "capabilities" ] decodeValue
                    , Decode.field "value" decodeValue
                    ]
                )
    in
    Decode.oneOf [ chrome, firefox ]
        |> reduceErrors


decodeAnswer : Decode.Decoder value -> Decoder (Answer value)
decodeAnswer decodeValue =
    let
        statusDecode =
            Decode.int

        sessionId =
            Decode.oneOf [ Decode.string, Decode.null "" ]

        chrome =
            Decode.field "status" statusDecode
                |> Decode.andThen
                    (\status ->
                        case status of
                            0 ->
                                Decode.map
                                    (\value ->
                                        { value = value
                                        }
                                    )
                                    (Decode.field "value" decodeValue)

                            _ ->
                                Decode.andThen Decode.fail decodeError
                    )

        firefox =
            Decode.map (\value -> { value = value })
                (Decode.oneOf
                    [ Decode.at [ "value", "capabilities" ] decodeValue
                    , Decode.field "value" decodeValue
                    ]
                )
    in
    Decode.oneOf [ chrome, firefox ]
        |> reduceErrors


decodeError : Decode.Decoder String
decodeError =
    Decode.at [ "value", "message" ] Decode.string


reduceErrors =
    Decode.maybe
        >> Decode.andThen
            (\a ->
                case a of
                    Just result ->
                        Decode.succeed result

                    Nothing ->
                        Decode.andThen Decode.fail decodeError
            )
