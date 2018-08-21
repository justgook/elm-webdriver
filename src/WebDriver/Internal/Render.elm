module WebDriver.Internal.Render exposing (LastResult(..), renderDot)

import Array exposing (Array)
import Dict
import NestedSet exposing (NestedSet)
import WebDriver.Internal exposing (Expectation(..), Node(..), Queue(..), TestStatus(..))
import WebDriver.Internal.Port as Port



-- http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html


type LastResult
    = Init
    | Good
    | Bad


renderDot : { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> LastResult -> Cmd msg
renderDot ({ queues, data, onlyMode } as input) lastResult =
    let
        formatError index error =
            let
                ( path, depth ) =
                    NestedSet.path index data
                        |> List.foldl
                            (\item ( acc, depth_ ) ->
                                case item of
                                    Text _ _ a ->
                                        ( acc ++ String.repeat depth_ "   " ++ a ++ "\n", depth_ + 1 )

                                    _ ->
                                        ( acc, depth_ )
                            )
                            ( "", 0 )

                depthSting =
                    String.repeat depth "   "
            in
            path ++ depthSting ++ replace "\n" ("\n" ++ depthSting) error

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
        Port.log ("\nRunning " ++ String.fromInt result.count ++ ", Skipping " ++ String.fromInt result.skipCount ++ " Tests: ")

    else if result.allDone then
        let
            fail =
                result.errors |> List.length
        in
        Cmd.batch
            [ Port.log
                ("\n\nPassed: "
                    ++ String.fromInt result.pass
                    ++ "\nSkiped: "
                    ++ String.fromInt result.skipCount
                    ++ "\nFail: "
                    ++ String.fromInt fail
                    ++ "\n\n\u{001B}[31m"
                    ++ (result.errors |> List.reverse |> String.join "\n\n")
                    ++ "\u{001B}[0m\n"
                )
            , dotOrF
                lastResult
            ]

    else
        dotOrF lastResult


dotOrF lastResult =
    if lastResult == Bad then
        Port.log "\u{001B}[31mF\u{001B}[0m"

    else
        Port.log "."
