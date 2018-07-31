module Main exposing (..)

import Test.Browsers
import Test.Steps exposing (suite)
import WebDriver.Runner as Runner exposing (run)
import WebDriver.Test exposing (concat)


main : Runner.TestRunner
main =
    concat [ Test.Browsers.suite, Test.Steps.suite ]
        |> run
