port module WebDriver.Internal.Port exposing (log, result)

import Json.Encode as E


port log : String -> Cmd msg


port result : E.Value -> Cmd msg
