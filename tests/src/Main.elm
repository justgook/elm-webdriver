module Main exposing (main)

-- import WebDriver.Runner as Runner exposing (TestRunner, configuration, runWith)
-- import WebDriver.Helper.Browser as Browser exposing (browsers)

import Array
import Json.Decode as D
import Json.Encode as E
import NestedSet exposing (NestedSet)
import Platform
import Port
import Task exposing (Task)
import Test.Steps
import WebDriver as WebDriver exposing (Test)
import WebDriver.Runner as Runner exposing (Browsers, Status(..))
import WebDriver.Runner.Helper as Runner


type alias Config =
    { capabilities : E.Value, url : String }


config : Config
config =
    { url =
        -- "http://localhost:4444/wd/hub"
        "http://localhost:9515"
    , capabilities =
        D.decodeString D.value
            ("{ \"desiredCapabilities\": {"
                ++ "\"browserName\": \"chrome\","
                ++ "\"version\": \"67.0\""
                ++ ",\"chromeOptions\": {\"args\": [ \"--headless\", \"--disable-gpu\", \"--window-size=800,600\" ]}"
                ++ "}}"
            )
            |> Result.withDefault E.null
    }


type alias Message =
    ( { testId : Int, queueId : Int }, Runner.Expectation )


type alias Model info =
    ( NestedSet String, Browsers info )


main : Platform.Program D.Value (Model Config) Message
main =
    Platform.worker
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        }


init flags =
    let
        ( m, cmds ) =
            Runner.init Test.Steps.suite config cmdFromTest (\e -> Port.log (ansi.red ++ e ++ ansi.reset))
                |> Runner.addParralel 0 9 cmdFromTest

        runCount1 =
            m |> Runner.countBy (\i_ -> i_.status == Running || i_.status == Waiting)

        skipCount1 =
            m |> Runner.countBy (.status >> (==) Skip)
    in
    ( m, Cmd.batch cmds )
        |> addCmd (startText runCount1 skipCount1)


update ( { testId, queueId }, expectation ) (( desc, tests ) as model) =
    Runner.next cmdFromTest expectation queueId testId model
        |> addCmd (dotOrF expectation)
        |> Runner.mapExit report expectation


report (( desc, _ ) as m) =
    let
        failCount =
            Runner.countBy isFail m

        reportText =
            -- Runner.collectBy isFail m
            Runner.collectBy (always True) m
                |> List.map
                    (\{ descId, status } ->
                        (NestedSet.path descId desc
                            |> String.join "."
                        )
                            ++ ": "
                            ++ stringFromStatus status
                    )
                |> String.join "\n"
    in
    Cmd.batch
        [ "\n Tests Failed: "
            ++ (failCount |> String.fromInt)
            ++ "\n "
            |> Port.log
        , Port.result (E.object [ ( "errors", E.list identity (List.repeat failCount E.null) ) ])
        , Port.log ("\n\n" ++ reportText)
        ]


isFail i =
    case i.status of
        Fail _ _ ->
            True

        _ ->
            False


startText runCount skipCount =
    Port.log
        ("\n"
            ++ ansi.green
            ++ "Running: "
            ++ ansi.reset
            ++ ansi.cyan
            ++ String.fromInt runCount
            ++ ansi.reset
            ++ ansi.green
            ++ ", Skipping: "
            ++ ansi.reset
            ++ ansi.cyan
            ++ String.fromInt skipCount
            ++ ansi.reset
            ++ ansi.green
            ++ " Tests: "
            ++ ansi.reset
        )


cmdFromTest queueId c =
    c.exec
        |> Runner.runOne c.info.url c.info.capabilities
        |> Task.perform (Tuple.pair { testId = c.testId, queueId = queueId })


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model, Cmd.batch [ cmd, oldCmd ] )


dotOrF result =
    case result of
        Err _ ->
            ansi.red ++ "F" ++ ansi.reset |> Port.log

        Ok _ ->
            ansi.green ++ "." ++ ansi.reset |> Port.log


stringFromStatus status =
    let
        replace a b c =
            String.split a c |> String.join b
    in
    case status of
        Pass _ ->
            ansi.green ++ "Pass" ++ ansi.reset

        Fail error _ ->
            ansi.red
                ++ "Fail\n"
                ++ ansi.reset
                ++ replace "\n" (ansi.reset ++ "\n" ++ ansi.red) (ansi.red ++ error ++ ansi.reset)

        Skip ->
            ansi.yellow ++ "Skip" ++ ansi.reset

        Waiting ->
            "Waiting"

        Running ->
            "Running"


ansi =
    { black = "\u{001B}[30m"
    , red = "\u{001B}[31m"
    , green = "\u{001B}[32m"
    , yellow = "\u{001B}[33m"
    , blue = "\u{001B}[34m"
    , magenta = "\u{001B}[35m"
    , cyan = "\u{001B}[36m"
    , white = "\u{001B}[37m"
    , reset = "\u{001B}[0m"
    }
