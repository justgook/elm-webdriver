module Test.Browsers exposing (suite)

import Task
import WebDriver as WebDriver exposing (describe, only, skip, test)
import WebDriver.Helper.Browser as Browser exposing (browsers)


suite : String -> WebDriver.Test
suite url =
    let
        { chrome, firefoxes, headlessChrome } =
            -- Browser.configure "http://localhost:4444/wd/hub"
            Browser.configure url
    in
    describe "Running in different browsers"
        [ browsers [ chrome, headlessChrome, firefoxes 3 ] <|
            describe "pack"
                [ test "Always succeed1" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed2" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed3" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed4" <| \{ url, getUrl, refresh } -> Task.succeed ()
                ]
        ]
