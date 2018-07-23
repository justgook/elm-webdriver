module WebDriver.Internal.Value.Timeouts exposing (Timeouts, decode, encode)

import Json.Decode exposing (field)
import Json.Encode


type alias Timeouts =
    { implicit : Int
    , pageLoad : Int
    , script : Int
    }


decode : Json.Decode.Decoder Timeouts
decode =
    Json.Decode.map3 Timeouts
        (field "implicit" Json.Decode.int)
        (field "pageLoad" Json.Decode.int)
        (field "script" Json.Decode.int)


encode : Timeouts -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "implicit", Json.Encode.int <| record.implicit )
        , ( "pageLoad", Json.Encode.int <| record.pageLoad )
        , ( "script", Json.Encode.int <| record.script )
        ]
