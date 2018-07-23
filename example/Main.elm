module Main exposing (..)

import Task
import WebDriver.Element as Selector
import WebDriver.Expect as Expect
import WebDriver.Runner as Runner exposing (run)
import WebDriver.Test as WebDriver exposing (describe, only, skip, test)


main : Runner.TestRunner
main =
    run suite


suite : WebDriver.Test
suite =
    describe "WebDriver"
        [ describe "LowLevel"
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
                ]
            ]
        , describe "Mock Html page"
            [ test "elment testing" <|
                \{ url, element, text } ->
                    url mock
                        |> Task.andThen (\_ -> "h1" |> Selector.css |> element)
                        |> Task.andThen (.value >> text)
                        |> Task.andThen (.value >> Expect.equal "Hello World")
            ]
        ]


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
"""
