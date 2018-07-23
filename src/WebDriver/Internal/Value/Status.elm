module WebDriver.Internal.Value.Status exposing (Status, decode, encode)

import Json.Decode exposing (field)
import Json.Encode


type alias Status =
    { build : ValueBuild
    , os : ValueOs
    }


type alias ValueBuild =
    { version : String
    }


type alias ValueOs =
    { arch : String
    , name : String
    , version : String
    }


decode : Json.Decode.Decoder Status
decode =
    Json.Decode.map2 Status
        (field "build" decodeValueBuild)
        (field "os" decodeValueOs)


decodeValueBuild : Json.Decode.Decoder ValueBuild
decodeValueBuild =
    Json.Decode.map ValueBuild
        (field "version" Json.Decode.string)


decodeValueOs : Json.Decode.Decoder ValueOs
decodeValueOs =
    Json.Decode.map3 ValueOs
        (field "arch" Json.Decode.string)
        (field "name" Json.Decode.string)
        (field "version" Json.Decode.string)


encode : Status -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "build", encodeValueBuild <| record.build )
        , ( "os", encodeValueOs <| record.os )
        ]


encodeValueBuild : ValueBuild -> Json.Encode.Value
encodeValueBuild record =
    Json.Encode.object
        [ ( "version", Json.Encode.string <| record.version )
        ]


encodeValueOs : ValueOs -> Json.Encode.Value
encodeValueOs record =
    Json.Encode.object
        [ ( "arch", Json.Encode.string <| record.arch )
        , ( "name", Json.Encode.string <| record.name )
        , ( "version", Json.Encode.string <| record.version )
        ]
