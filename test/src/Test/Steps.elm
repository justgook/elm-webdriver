module Test.Steps exposing (suite)

import Task
import WebDriver as WebDriver exposing (describe, only, skip, test)
import WebDriver.Expect as Expect
import WebDriver.Step.Element as Selector


suite : WebDriver.Test
suite =
    describe "WebDriver"
        [ describe "Functions"
            [ test "url / refresh / getUrl" <|
                \{ url, getUrl, refresh } ->
                    url blankPage
                        |> Task.andThen (\_ -> refresh)
                        |> Task.andThen (\_ -> getUrl)
                        |> Task.andThen
                            (\({ value } as data) ->
                                Expect.equal value blankPage
                            )
            , test "url / back / forward / getUrl" <|
                \{ url, getUrl, back, forward } ->
                    url blankPage
                        |> Task.andThen (\_ -> back)
                        |> Task.andThen (\_ -> forward)
                        |> Task.andThen (\_ -> getUrl)
                        |> Task.andThen
                            (\({ value } as data) ->
                                Expect.equal value blankPage
                            )
            , test "url / title" <|
                \{ url, title } ->
                    url tiltlePage
                        |> Task.andThen (\_ -> title)
                        |> Task.andThen
                            (\({ value } as data) ->
                                String.contains "Hello" value
                                    |> Expect.true ("Expected the title contains text Hello, but it is `" ++ value ++ "`")
                            )

            -- , only <|
            , describe "elemnt properties"
                [ test "element, text" <|
                    \{ url, element, text } ->
                        url mock
                            |> Task.andThen (\_ -> "h1" |> Selector.css |> element)
                            |> Task.andThen (.value >> text)
                            |> Task.andThen (.value >> Expect.equal "Hello World")
                , test "elements" <|
                    \{ url, elements } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "selected" <|
                    \{ url, selected } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "enabled" <|
                    \{ url, enabled } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "tagName" <|
                    \{ url, tagName } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "attribute" <|
                    \{ url, attribute } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "property" <|
                    \{ url, property } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "css" <|
                    \{ url, css } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "elementInElement" <|
                    \{ url, elementInElement } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                , test "elementsInElement" <|
                    \{ url, elementsInElement } -> url mock |> Task.andThen (\_ -> Task.succeed ())
                ]
            , test "getWindowRect" <|
                \{ url, getWindowRect } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "setWindowRect" <|
                \{ url, setWindowRect } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "rect" <|
                \{ url, rect } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "click" <|
                \{ url, click } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "clear" <|
                \{ url, clear } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "execute" <|
                \{ url, execute } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "executeAsync" <|
                \{ url, executeAsync } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "cookies" <|
                \{ url, cookies } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "cookie" <|
                \{ url, cookie } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "deleteCookies" <|
                \{ url, deleteCookies } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "deleteCookie" <|
                \{ url, deleteCookie } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "addCookie" <|
                \{ url, addCookie } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "promptText" <|
                \{ url, promptText } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "alertText" <|
                \{ url, alertText } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "alertDismiss" <|
                \{ url, alertDismiss } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "alertAccept" <|
                \{ url, alertAccept } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "screenshot" <|
                \{ url, screenshot } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "elementScreenshot" <|
                \{ url, elementScreenshot } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "actions" <|
                \{ url, actions } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "release" <|
                \{ url, release } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "status" <|
                \{ url, status } ->
                    url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "getTimeouts" <|
                \{ url, getTimeouts } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "setTimeouts" <|
                \{ url, setTimeouts } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "windowHandle" <|
                \{ url, windowHandle } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "windowHandles" <|
                \{ url, windowHandles } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "close" <|
                \{ url, close } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "window" <|
                \{ url, window } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "fullscreen" <|
                \{ url, fullscreen } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "maximize" <|
                \{ url, maximize } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "minimize" <|
                \{ url, minimize } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "frameParent" <|
                \{ url, frameParent } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            , test "frame" <|
                \{ url, frame } -> url mock |> Task.andThen (\_ -> Task.succeed ())
            ]
        ]



-- VirtualDom.on : String -> Handler msg -> Attribute msg


blankPage : String
blankPage =
    "data:text/plain,Hello"


tiltlePage : String
tiltlePage =
    """data:text/html,<title>Hello World</title>"""


mock : String
mock =
    """data:text/html,
    <h1>Hello World</h1>
    <h1>Hello World2</h1>
"""
