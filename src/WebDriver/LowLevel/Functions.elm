module WebDriver.LowLevel.Functions
    exposing
        ( Functions
        , functions
        , sessionStart
        , sessionStop
        , status
        , url
        , getUrl
        )

import Task exposing (Task)
import WebDriver.LowLevel.Value as Value exposing (Value, Answer, answerDecoder)
import Json.Encode as Encode
import WebDriver.LowLevel.HttpHelper as Http exposing (toTask)
import Http
import WebDriver.LowLevel.Value.Timeouts as Timeouts


type alias Functions =
    { fail : String -> Task String Answer
    , continue : Answer -> Task String Answer
    , url : String -> Task String Answer
    , status : Task String Answer
    , getUrl : Task String Answer
    , getTimeouts : Task String Answer
    , setTimeouts : Timeouts.Value -> Task String Answer
    , back : Task String Answer
    , forward : Task String Answer
    , refresh : Task String Answer
    , title : Task String Answer
    , windowHandle : Task String Answer
    , windowHandles : Task String Answer
    , close : Task String Answer
    }


{-| <https://w3c.github.io/webdriver/#list-of-endpoints>
-}
functions : String -> String -> Functions
functions driverUrl sessionId =
    let
        options =
            { driver = { url = driverUrl }, sessionId = sessionId }
    in
        { fail = Task.fail
        , continue = \data -> Task.succeed data
        , url = \data -> url options data |> toTask
        , status = status options |> toTask
        , getUrl = getUrl options |> toTask
        , getTimeouts = getTimeouts options |> toTask
        , setTimeouts = \data -> setTimeouts options data |> toTask
        , back = back options |> toTask
        , forward = forward options |> toTask
        , refresh = refresh options |> toTask
        , title = title options |> toTask
        , windowHandle = windowHandle options |> toTask
        , windowHandles = windowHandles options |> toTask
        , close = close options |> toTask
        }


fail =
    ""


continue =
    ""


{-| -}
sessionStart : { b | driver : { a | url : String } } -> Encode.Value -> Http.Request Answer
sessionStart settings capabilities =
    capabilities
        |> Http.jsonBody
        |> \body ->
            Http.post (settings.driver.url ++ "/session") body answerDecoder.sessionStart


{-| -}
sessionStop : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
sessionStop settings =
    Http.delete (settings.driver.url ++ "/session/" ++ settings.sessionId)
        answerDecoder.sessionStop


{-| Query the server’s current status. The server should respond with a general “HTTP 200 OK” response if it is alive and accepting commands. The response body should be a JSON object describing the state of the server. All server implementations should return two basic objects describing the server’s current platform and when the server was built. All fields are optional; if omitted, the client should assume the value is unknown. Furthermore, server implementations may include additional fields not listed here.
-}
status : { b | driver : { a | url : String } } -> Http.Request Answer
status settings =
    Http.get (settings.driver.url ++ "/status") answerDecoder.status


{-| -}
getTimeouts : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
getTimeouts settings =
    Http.get (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/timeouts")
        answerDecoder.getTimeouts


{-| Configure the amount of time that a particular type of operation can execute for before they are aborted and a Timeout error is returned to the client.

    * `script` - session script timeout - Determines when to interrupt a script that is being evaluated.
    * `pageLoad` - session page load timeout - Provides the timeout limit used to interrupt navigation of the browsing context.
    * `implicit`- session implicit wait timeout - Gives the timeout of when to abort locating an element.

-}
setTimeouts : { b | driver : { a | url : String }, sessionId : String } -> Timeouts.Value -> Http.Request Answer
setTimeouts settings value =
    Encode.object
        [ ( "script", Encode.int value.script )
        , ( "pageLoad", Encode.int value.pageLoad )
        , ( "implicit", Encode.int value.implicit )
        ]
        |> Http.jsonBody
        |> \body ->
            Http.post (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/timeouts")
                body
                answerDecoder.setTimeouts


{-| Load the URL of the browser.
-}
url : { b | driver : { a | url : String }, sessionId : String } -> String -> Http.Request Answer
url settings url_ =
    Encode.object
        [ ( "url"
          , Encode.string url_
          )
        ]
        |> Http.jsonBody
        |> \body ->
            Http.post (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/url")
                body
                answerDecoder.url


{-| Get the url of current opened website.
-}
getUrl : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
getUrl settings =
    Http.get (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/url")
        answerDecoder.getUrl


history_ : String -> { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
history_ path settings =
    Http.post (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/" ++ path)
        Http.emptyBody
        answerDecoder.url


{-| Navigate backwards in the browser history, if possible.
-}
back : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
back =
    history_ "/back"


{-| Navigate forwards in the browser history, if possible.
-}
forward : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
forward =
    history_ "/forward"


{-| Refresh the current page.
-}
refresh : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
refresh =
    history_ "/refresh"


{-| Get the title of current opened website.
-}
title : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
title settings =
    Http.get (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/title")
        answerDecoder.title


{-| Get handle with tabs in the browser.
-}
windowHandle : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
windowHandle settings =
    Http.get (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/window")
        answerDecoder.windowHandle


{-| Retrieve the list of all window handles available to the session.
-}
windowHandles : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
windowHandles settings =
    Http.get (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/window/handles")
        answerDecoder.windowHandles


{-| Close current window (and focus on an other window).
If no window handle is given it automatically switches back to the first handle.
-}
close : { b | driver : { a | url : String }, sessionId : String } -> Http.Request Answer
close settings =
    Http.delete (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "window")
        answerDecoder.windowHandleClose


switch settings windowHandle =
    Encode.object
        [ ( "url"
          , Encode.string windowHandle
          )
        ]
        |> Http.jsonBody
        |> \body ->
            Http.post (settings.driver.url ++ "/session/" ++ settings.sessionId ++ "/url")
                body
                answerDecoder.url



-- windowHandleFullscreen
-- windowHandleMaximize
-- windowHandlePosition
-- windowHandleSize
-- windowHandles
