module Test.Browsers exposing (suite)

import Task
import WebDriver as WebDriver exposing (describe, only, skip, test)
import WebDriver.Helper.Browser as Browser exposing (browsers)


suite : WebDriver.Test
suite =
    -- only <|
    describe "Running in different browsers"
        [ browsers [ Browser.chrome, Browser.headlessChrome, Browser.firefoxes 3 ] <|
            describe "pack"
                [ test "Always succeed1" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed2" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed3" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed4" <| \{ url, getUrl, refresh } -> Task.succeed ()
                ]
        ]
