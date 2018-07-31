module Test.Browsers exposing (suite)

import Task
import WebDriver.Browser as Browser exposing (browsers)
import WebDriver.Test as WebDriver exposing (describe, only, skip, test)


suite : WebDriver.Test
suite =
    -- only <|
    describe "Running in different browsers"
        [ browsers [ Browser.chrome, Browser.headlessChrome, Browser.firefox ] <|
            describe "pack"
                [ test "Always succeed1" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed2" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , skip <| test "Always succeed3" <| \{ url, getUrl, refresh } -> Task.succeed ()
                , test "Always succeed4" <| \{ url, getUrl, refresh } -> Task.succeed ()
                ]
        ]
