module WebDriver.LowLevel
    exposing
        ( browser
        , Expectation(..)
        )

import Task exposing (Task)
import WebDriver.LowLevel.HttpHelper exposing (toTask)
import WebDriver.LowLevel.HttpHelper as Http
import WebDriver.LowLevel.Functions exposing (Functions, functions, sessionStart, sessionStop)
import Json.Encode as Encode


type Expectation
    = Pass
    | Fail Bool {- Exit -} String


browser : String -> Encode.Value -> (Functions -> Task String a) -> Task Never Expectation
browser driverUrl capabilities tests =
    let
        stop sessionId =
            sessionStop { driver = { url = driverUrl }, sessionId = sessionId }
                |> toTask
    in
        sessionStart { driver = { url = driverUrl } } capabilities
            |> toTask
            |> Task.andThen
                (\{ sessionId } ->
                    functions driverUrl sessionId
                        |> tests
                        |> Task.andThen (\_ -> stop sessionId)
                        |> Task.andThen (\_ -> Task.succeed Pass)
                        |> Task.onError
                            (\err ->
                                (stop sessionId)
                                    |> Task.andThen (\_ -> Task.succeed (Fail False err))
                            )
                )
            |> Task.onError
                (\err ->
                    Task.succeed
                        (Fail True ("Problem with Webdriver host (" ++ err ++ ")"))
                )
