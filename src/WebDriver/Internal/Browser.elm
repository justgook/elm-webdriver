module WebDriver.Internal.Browser exposing (browser)

import Json.Encode as Encode
import Task exposing (Task)
import WebDriver.Internal exposing (Expectation(..))
import WebDriver.Internal.HttpHelper as Http exposing (toTask)
import WebDriver.Step exposing (Functions, functions, sessionStart, sessionStop)


browser : String -> Encode.Value -> (Functions -> Task Never Expectation) -> Task Never Expectation
browser driverUrl capabilities tests =
    let
        stop_ =
            stop driverUrl
    in
    sessionStart { url = driverUrl } capabilities
        |> toTask
        |> Task.andThen
            (\{ sessionId } ->
                SessionIdHandler sessionId
                    |> Task.succeed
            )
        |> Task.onError
            (\err ->
                ErrorHandler ("Problem with Webdriver host (" ++ err ++ ")")
                    |> Task.succeed
            )
        |> Task.andThen
            (\result ->
                case result of
                    SessionIdHandler sessionId ->
                        let
                            func =
                                functions { url = driverUrl, sessionId = sessionId }
                        in
                        tests func
                            |> Task.andThen (stop_ sessionId)

                    ErrorHandler err ->
                        Fail True err
                            |> Task.succeed
            )


type TaskStateHandler
    = SessionIdHandler String
    | ErrorHandler String


stop : String -> String -> Expectation -> Task Never Expectation
stop driverUrl sessionId result =
    sessionStop { url = driverUrl, sessionId = sessionId }
        |> toTask
        |> Task.andThen (\_ -> Task.succeed result)
        |> Task.onError
            (\err ->
                Task.succeed
                    (Fail True ("Problem with Webdriver host (" ++ err ++ ")"))
            )
