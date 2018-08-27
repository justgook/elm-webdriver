module WebDriver.Internal.Browser exposing (browser)

import Json.Encode as E
import Process
import Task exposing (Task)
import WebDriver.Internal exposing (Expectation)
import WebDriver.Internal.HttpHelper as Http
import WebDriver.Step exposing (Functions, functions, sessionStart, sessionStop)


browser : String -> E.Value -> (Functions -> Task Never (Result String ())) -> Task Never Expectation
browser driverUrl capabilities tests =
    let
        stop sessionId value result =
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
                                    , capabilities = value
                                    }
                                )
                            |> Task.succeed
                    )
                |> Task.onError
                    (\err ->
                        Task.succeed
                            (Err { critical = True, error = "Problem STOP Webdriver host (" ++ err ++ ")", capabilities = value })
                    )

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
        -- Retry after 3ms if first connect is unsuccessful
        |> Task.onError (\_ -> Process.sleep 3 |> Task.andThen (always restart))
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
                            |> Task.andThen (stop sessionId value)

                    ErrorHandler err value ->
                        Err { critical = True, error = err, capabilities = value }
                            |> Task.succeed
            )


type TaskStateHandler
    = SessionIdHandler String E.Value
    | ErrorHandler String E.Value



-- stop : String -> String -> Expectation -> Task Never Expectation
-- stop driverUrl sessionId result =
--     sessionStop { url = driverUrl, sessionId = sessionId }
--         |> toTask
--         |> Task.andThen (\_ -> Task.succeed result)
--         |> Task.onError
--             (\err ->
--                 Task.succeed
--                     (Fail True ("Problem with Webdriver host (" ++ err ++ ")"))
--             )
