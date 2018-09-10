module WebDriver.Step.Selenium exposing (execute, executeAsync)

{-| Add suport to outdated Selenium Wire Protocol to be able suport [macOs Webdriver](https://developer.apple.com/documentation/webkit/macos_webdriver_commands_for_safari)

@docs execute, executeAsync

-}

import Json.Encode as Json
import WebDriver.Internal.Value as Value exposing (Answer, Cookie, Out, WindowHandle(..), answerDecoder, decodeAnswerWithSession, jsonFromSelector)
import WebDriver.Step.Internal exposing (execute_)


type alias WithSession =
    Value.WithSession


{-| Same as [`Step.execute`](../#execute)
-}
executeAsync : WithSession -> String -> List Json.Value -> Out Json.Value
executeAsync =
    execute_ "/execute_async"


{-| Same as [`Step.execute`](../#execute)
-}
execute : WithSession -> String -> List Json.Value -> Out Json.Value
execute =
    execute_ "/execute"
