module WebDriver.Internal.Report exposing (LastResult(..), render)

import Array exposing (Array)
import Dict
import Json.Decode as D
import Json.Encode as E
import NestedSet exposing (NestedSet)
import WebDriver.Internal exposing (Expectation(..), Node(..), Queue(..), TestStatus(..))
import WebDriver.Internal.Port as Port



-- http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html


type LastResult
    = Init
    | Good
    | Bad


render : { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> LastResult -> Cmd msg
render ({ queues, data, onlyMode } as input) lastResult =
    let
        formatError index error =
            let
                ( path, depth ) =
                    NestedSet.path index data
                        |> List.foldl
                            (\item ( acc, depth_ ) ->
                                case item of
                                    Text _ _ testText ->
                                        ( acc ++ String.repeat depth_ "   " ++ ansi.cyan ++ testText ++ ansi.reset ++ "\n", depth_ + 1 )

                                    _ ->
                                        ( acc, depth_ )
                            )
                            ( "", 0 )

                depthSting =
                    String.repeat depth "   "
            in
            path ++ depthSting ++ replace "\n" (ansi.reset ++ "\n" ++ ansi.red ++ depthSting) (ansi.red ++ error ++ ansi.reset)

        replace a b c =
            String.split a c |> String.join b

        result =
            NestedSet.indexedFoldr
                (\index item acc ->
                    case item of
                        Test { status } ->
                            status
                                |> Dict.toList
                                |> List.foldr
                                    (\( _, item_ ) ({ output, firstRun, allDone } as acc_) ->
                                        case item_ of
                                            Done (Fail critical {- Exit -} error) ->
                                                { acc_
                                                    | firstRun = False
                                                    , critical = critical || acc_.critical
                                                    , errors = formatError index error :: acc_.errors
                                                }

                                            Done Pass ->
                                                { acc_
                                                    | firstRun = False
                                                    , count = acc_.count + 1
                                                    , pass = acc_.pass + 1
                                                    , success = acc_.success + 1
                                                }

                                            Running ->
                                                { acc_ | count = acc_.count + 1, allDone = False }

                                            InQueue ->
                                                { acc_ | count = acc_.count + 1 }

                                            OnlyModeSkip ->
                                                { acc_ | skipCount = acc_.skipCount + 1 }

                                            Skip ->
                                                { acc_ | skipCount = acc_.skipCount + 1 }
                                    )
                                    acc

                        _ ->
                            acc
                )
                { count = 0
                , pass = 0
                , skipCount = 0
                , output = ""
                , errors = []
                , critical = False
                , success = 0
                , firstRun = True
                , allDone = True
                }
                data
    in
    if result.firstRun then
        Port.log
            ("\n"
                ++ ansi.green
                ++ "Running: "
                ++ ansi.reset
                ++ ansi.cyan
                ++ String.fromInt result.count
                ++ ansi.reset
                ++ ansi.green
                ++ ", Skipping: "
                ++ ansi.reset
                ++ ansi.cyan
                ++ String.fromInt result.skipCount
                ++ ansi.reset
                ++ ansi.green
                ++ " Tests: "
                ++ ansi.reset
            )

    else if result.allDone then
        let
            fail =
                result.errors |> List.length
        in
        Cmd.batch
            [ makeReport result.errors queues data |> Port.result
            , Port.log
                ("\n\n"
                    ++ ansi.reset
                    ++ ansi.green
                    ++ "Passed: "
                    ++ ansi.reset
                    ++ ansi.cyan
                    ++ String.fromInt result.pass
                    ++ ansi.reset
                    ++ ansi.yellow
                    ++ "\nSkiped: "
                    ++ ansi.reset
                    ++ ansi.cyan
                    ++ String.fromInt result.skipCount
                    ++ ansi.reset
                    ++ ansi.red
                    ++ "\nFail:   "
                    ++ ansi.reset
                    ++ ansi.cyan
                    ++ String.fromInt fail
                    ++ ansi.reset
                    ++ "\n\n"
                    ++ (result.errors |> List.reverse |> String.join "\n")
                    ++ "\u{001B}[0m\n"
                )
            , dotOrF
                lastResult
            ]

    else
        dotOrF lastResult


makeReport errors queues data =
    let
        result =
            data
                |> NestedSet.indexedFoldr
                    (\index value acc ->
                        case value of
                            Test { status } ->
                                ( NestedSet.path index data |> List.foldl pathToStringAppender "", status |> Dict.values |> E.list statusToValue ) :: acc

                            _ ->
                                acc
                    )
                    []
    in
    result
        |> (::) ( "errors", E.list E.string errors )
        |> List.reverse
        |> E.object


statusToValue status =
    case status of
        Done (Fail critical {- Exit -} error) ->
            E.string "fail"

        Done Pass ->
            E.string "pass"

        Running ->
            E.string "skip"

        InQueue ->
            E.string "skip"

        OnlyModeSkip ->
            E.string "skip"

        Skip ->
            E.string "skip"


pathToStringAppender item itemString =
    case item of
        Text _ _ t ->
            if itemString == "" then
                t

            else
                itemString ++ " -> " ++ t

        _ ->
            itemString


nodeEncoder : Array Queue -> Node -> E.Value
nodeEncoder queues _ =
    E.null


dotOrF lastResult =
    if lastResult == Bad then
        ansi.red ++ "F" ++ ansi.reset |> Port.log

    else
        ansi.green ++ "." ++ ansi.reset |> Port.log


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
