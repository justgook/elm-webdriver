module WebDriver.Internal.Browser exposing (Expectation, browser)

import Json.Encode as E
import Process
import Task exposing (Task)
import WebDriver.Internal exposing (UnitTestFunction)
import WebDriver.Internal.HttpHelper as Http
import WebDriver.Step exposing (Functions, functions, sessionStart, sessionStop)


type TaskStateHandler
    = SessionIdHandler String E.Value
    | ErrorHandler String E.Value


type alias Expectation =
    Result { critical : Bool, error : String, context : E.Value } E.Value


browser : String -> E.Value -> UnitTestFunction -> Task Never Expectation
browser driverUrl capabilities tests =
    let
        restart =
            sessionStart { url = driverUrl } capabilities
                |> Http.toTask
                |> Task.andThen
                    (\{ sessionId, value } ->
                        SessionIdHandler sessionId value
                            |> Task.succeed
                    )

        exitOnError value =
            Task.onError
                (\err ->
                    ErrorHandler ("Problem START Webdriver host (" ++ err ++ ")") value
                        |> Task.succeed
                )
    in
    restart
        -- Retry after 300ms if first connect is unsuccessful
        |> Task.onError (\_ -> Process.sleep 300 |> Task.andThen (always restart))
        |> exitOnError E.null
        |> Task.andThen
            (\result ->
                case result of
                    SessionIdHandler sessionId value ->
                        let
                            func =
                                functions { url = driverUrl, sessionId = sessionId }
                        in
                        tests func
                            |> Task.andThen (stop driverUrl sessionId value)

                    ErrorHandler err value ->
                        Err { critical = True, error = err, context = value }
                            |> Task.succeed
            )


stop driverUrl sessionId value result =
    sessionStop { url = driverUrl, sessionId = sessionId }
        |> Http.toTask
        |> Task.andThen
            (\_ ->
                result
                    |> Result.map (always value)
                    |> Result.mapError
                        (\error ->
                            { critical = False
                            , error = error
                            , context = value
                            }
                        )
                    |> Task.succeed
            )
        |> Task.onError
            (\err ->
                Task.succeed
                    (Err { critical = True, error = "Problem STOP Webdriver host (" ++ err ++ ")", context = value })
            )
