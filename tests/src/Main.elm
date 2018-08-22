module Main exposing (main)

import Test.Steps
import WebDriver as WebDriver exposing (Test, concat, describe, only, skip, test)
import WebDriver.Helper.Browser as Browser exposing (browsers)
import WebDriver.Runner as Runner exposing (TestRunner, configuration, runWith)


url : String
url =
    -- "http://localhost:4444/wd/hub"
    "http://localhost:9515"


runInBrowsers : Test -> Test
runInBrowsers =
    let
        { chrome, firefoxes, headlessChromes, chromes } =
            Browser.configure url
    in
    browsers [ headlessChromes 12 ]


main : TestRunner
main =
    runInBrowsers Test.Steps.suite
        |> runWith configuration
