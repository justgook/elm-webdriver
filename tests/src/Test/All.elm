module Test.All exposing (suite)

import Test.Action
import Test.Step
import WebDriver exposing (describe)


suite : WebDriver.Test a
suite =
    describe "WebDriver"
        [ describe "Step" (Test.Step.tests ++ Test.Action.tests) ]
