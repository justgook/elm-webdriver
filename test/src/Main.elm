module Main exposing (main)

import Test.Browsers
import Test.Steps
import WebDriver as WebDriver exposing (Test, concat, describe, only, skip, test)
import WebDriver.Helper.Browser as Browser exposing (browsers)
import WebDriver.Runner as Runner exposing (TestRunner, configuration, runWith)


url : String
url =
    "http://localhost:4444/wd/hub"



-- "http://localhost:9515"


{ chrome, firefoxes, headlessChrome, chromes } =
    Browser.configure url


runInBrowsers : Test -> Test
runInBrowsers =
    browsers [ chromes 12 ]


main : TestRunner
main =
    concat
        [ skip <| Test.Browsers.suite url
        , runInBrowsers <| Test.Steps.suite
        ]
        |> runWith { configuration | reporter = Runner.SpecLiveReporter }
