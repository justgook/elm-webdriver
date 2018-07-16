module WebDriver.LowLevel.Value.SessionDelme exposing (decode, Value)

import Json.Decode exposing (field)
import Json.Decode.Pipeline
import Json.Encode as Encode


type alias SessionValueChrome =
    { chromedriverVersion : String
    , userDataDir : String
    }


type alias Value =
    { acceptInsecureCerts : Bool
    , acceptSslCerts : Bool
    , applicationCacheEnabled : Bool
    , browserConnectionEnabled : Bool
    , browserName : String
    , chrome : SessionValueChrome
    , cssSelectorsEnabled : Bool
    , databaseEnabled : Bool
    , handlesAlerts : Bool
    , hasTouchScreen : Bool
    , javascriptEnabled : Bool
    , locationContextEnabled : Bool
    , mobileEmulationEnabled : Bool
    , nativeEvents : Bool
    , networkConnectionEnabled : Bool
    , pageLoadStrategy : String
    , platform : String
    , rotatable : Bool
    , setWindowRect : Bool
    , takesHeapSnapshot : Bool
    , takesScreenshot : Bool
    , unexpectedAlertBehaviour : String
    , version : String
    , webStorageEnabled : Bool
    }


decodeSessionValueChrome : Json.Decode.Decoder SessionValueChrome
decodeSessionValueChrome =
    Json.Decode.map2 SessionValueChrome
        (field "chromedriverVersion" Json.Decode.string)
        (field "userDataDir" Json.Decode.string)


decode : Json.Decode.Decoder Value
decode =
    Json.Decode.Pipeline.decode Value
        |> Json.Decode.Pipeline.required "acceptInsecureCerts" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "acceptSslCerts" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "applicationCacheEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "browserConnectionEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "browserName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "chrome" (decodeSessionValueChrome)
        |> Json.Decode.Pipeline.required "cssSelectorsEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "databaseEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "handlesAlerts" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "hasTouchScreen" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "javascriptEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "locationContextEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "mobileEmulationEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "nativeEvents" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "networkConnectionEnabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "pageLoadStrategy" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "platform" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "rotatable" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "setWindowRect" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "takesHeapSnapshot" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "takesScreenshot" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "unexpectedAlertBehaviour" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "version" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "webStorageEnabled" (Json.Decode.bool)


encodeSessionValueChrome : SessionValueChrome -> Encode.Value
encodeSessionValueChrome record =
    Encode.object
        [ ( "chromedriverVersion", Encode.string <| record.chromedriverVersion )
        , ( "userDataDir", Encode.string <| record.userDataDir )
        ]


encode : Value -> Encode.Value
encode record =
    Encode.object
        [ ( "acceptInsecureCerts", Encode.bool <| record.acceptInsecureCerts )
        , ( "acceptSslCerts", Encode.bool <| record.acceptSslCerts )
        , ( "applicationCacheEnabled", Encode.bool <| record.applicationCacheEnabled )
        , ( "browserConnectionEnabled", Encode.bool <| record.browserConnectionEnabled )
        , ( "browserName", Encode.string <| record.browserName )
        , ( "chrome", encodeSessionValueChrome <| record.chrome )
        , ( "cssSelectorsEnabled", Encode.bool <| record.cssSelectorsEnabled )
        , ( "databaseEnabled", Encode.bool <| record.databaseEnabled )
        , ( "handlesAlerts", Encode.bool <| record.handlesAlerts )
        , ( "hasTouchScreen", Encode.bool <| record.hasTouchScreen )
        , ( "javascriptEnabled", Encode.bool <| record.javascriptEnabled )
        , ( "locationContextEnabled", Encode.bool <| record.locationContextEnabled )
        , ( "mobileEmulationEnabled", Encode.bool <| record.mobileEmulationEnabled )
        , ( "nativeEvents", Encode.bool <| record.nativeEvents )
        , ( "networkConnectionEnabled", Encode.bool <| record.networkConnectionEnabled )
        , ( "pageLoadStrategy", Encode.string <| record.pageLoadStrategy )
        , ( "platform", Encode.string <| record.platform )
        , ( "rotatable", Encode.bool <| record.rotatable )
        , ( "setWindowRect", Encode.bool <| record.setWindowRect )
        , ( "takesHeapSnapshot", Encode.bool <| record.takesHeapSnapshot )
        , ( "takesScreenshot", Encode.bool <| record.takesScreenshot )
        , ( "unexpectedAlertBehaviour", Encode.string <| record.unexpectedAlertBehaviour )
        , ( "version", Encode.string <| record.version )
        , ( "webStorageEnabled", Encode.bool <| record.webStorageEnabled )
        ]
