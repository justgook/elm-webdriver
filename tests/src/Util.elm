port module Util exposing (msgFromTask, printDotOrF, printError, report)

import Json.Encode as E
import Task exposing (Task)
import WebDriver.Setup exposing (Reference, Report, Status(..), SuiteState, isFail, next, setup)


port log : String -> Cmd msg


port result : E.Value -> Cmd msg


report : Report info -> Cmd msg
report data =
    let
        outputText =
            data
                |> List.map
                    (\{ desc, status } ->
                        String.join "." desc
                            ++ ": "
                            ++ reportStatus status
                    )
                |> String.join "\n"
                |> (++) "\n\n"
    in
    Cmd.batch
        [ log (outputText ++ "\n")
        , reportEncode data |> result
        ]


reportEncode : Report info -> E.Value
reportEncode =
    let
        statusEncode s =
            E.string
                (case s of
                    Pass ->
                        "✓"

                    Fail e ->
                        "✘"

                    Skip ->
                        "▶▶"
                )
    in
    List.map (\{ desc, status } -> ( String.join "." desc, statusEncode status ))
        >> E.object


reportStatus : Status -> String
reportStatus s =
    case s of
        Pass ->
            ansi.green ++ "Pass" ++ ansi.reset

        Fail e ->
            printError_ e

        Skip ->
            ansi.yellow ++ "Skip" ++ ansi.reset


msgFromTask : List (Task Never msg) -> Cmd msg
msgFromTask =
    List.map (Task.perform identity) >> Cmd.batch


replace : String -> String -> String -> String
replace a b c =
    String.split a c |> String.join b


printError_ : String -> String
printError_ error =
    ansi.red
        ++ "Fail\n"
        ++ ansi.reset
        ++ replace "\n" (ansi.reset ++ "\n" ++ ansi.red) (ansi.red ++ error ++ ansi.reset)


printError : String -> Cmd cmd
printError =
    printError_ >> (++) "\n" >> log


printDotOrF : Reference info -> ( model, Cmd msg ) -> ( model, Cmd msg )
printDotOrF ref =
    (if isFail ref then
        ansi.red ++ "F" ++ ansi.reset

     else
        ansi.green ++ "." ++ ansi.reset
    )
        |> log
        |> addCmd


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model, Cmd.batch [ cmd, oldCmd ] )


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
