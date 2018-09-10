module WebDriver.Step.Internal exposing (execute_)

import Http
import Json.Encode as Json
import WebDriver.Internal.Value as Value exposing (Out, WithSession, answerDecoder)


execute_ : String -> WithSession -> String -> List Json.Value -> Out Json.Value
execute_ path settings function args =
    -- W3C !STANDARD! Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/execute/sync")
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ path)
        (Json.object
            [ ( "script"
              , Json.string function
              )
            , ( "args"
              , Json.list identity args
              )
            ]
            |> Http.jsonBody
        )
        answerDecoder.value
