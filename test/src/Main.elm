module Main exposing (..)

import Test.Browsers
import Test.Steps exposing (suite)
import WebDriver exposing (concat)
import WebDriver.Runner as Runner exposing (run)


main : Runner.TestRunner
main =
    concat [ Test.Browsers.suite, Test.Steps.suite ]
        |> run
