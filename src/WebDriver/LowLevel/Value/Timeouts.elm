module WebDriver.LowLevel.Value.Timeouts exposing (..)

import Json.Encode
import Json.Decode exposing (field)


type alias Value =
    { implicit : Int
    , pageLoad : Int
    , script : Int
    }


decode : Json.Decode.Decoder Value
decode =
    Json.Decode.map3 Value
        (field "implicit" Json.Decode.int)
        (field "pageLoad" Json.Decode.int)
        (field "script" Json.Decode.int)


encode : Value -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "implicit", Json.Encode.int <| record.implicit )
        , ( "pageLoad", Json.Encode.int <| record.pageLoad )
        , ( "script", Json.Encode.int <| record.script )
        ]
