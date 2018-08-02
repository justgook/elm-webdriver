module Main exposing (main)

import Test.Browsers
import Test.Steps
import WebDriver exposing (concat, only)
import WebDriver.Runner as Runner exposing (TestRunner, configuration, runWith)


main : TestRunner
main =
    concat
        [ Test.Browsers.suite
        , Test.Steps.suite
        ]
        |> runWith { configuration | reporter = Runner.DotReporter }
