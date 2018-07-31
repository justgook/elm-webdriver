module WebDriver.Internal.Render exposing (..)

import Ansi exposing (Color(..))
import Ansi.Compose exposing (Style, div, foreground, renderWindow, span, text, toModel)
import Ansi.Log exposing (init)
import Array exposing (Array)
import Dict
import NestedSet exposing (NestedSet)
import WebDriver.Internal exposing (Expectation(..), Node(..), Queue(..), TestStatus(..))
import WebDriver.Internal.Port as Log


-- render : { a | data : NestedSet Node, queues : Array Queue } -> Cmd msg


render : { a | data : NestedSet Node, queues : Array Queue } -> Cmd msg
render ({ queues, data } as input) =
    let
        render_ index node acc =
            let
                depth =
                    NestedSet.depth index data

                nextNodes =
                    stringFromNode queues node depth
            in
            acc ++ nextNodes

        result =
            init Ansi.Log.Raw
                |> toModel (NestedSet.indexedFoldr render_ [] data)
                |> renderWindow
    in
    [ Log.log result
    , Log.log "\x1B[2J\x1B[1;1H"
    ]
        |> Cmd.batch


status2string depth queues ( queueID, status ) =
    let
        browserName =
            case Array.get queueID queues of
                Just (Queue ( { name }, _ )) ->
                    name

                Nothing ->
                    "Unknown"

        offset =
            String.repeat depth "  "
    in
    case status of
        Done (Fail critical {- Exit -} error) ->
            -- toString queueID ++ "(" ++ toString error ++ ")"
            offset ++ "✘ Fail in " ++ browserName ++ " (" ++ error ++ ")\n" |> span errorStyle

        Done Pass ->
            offset ++ "✔ Success in " ++ browserName ++ "\n" |> span successStyle

        OnlyModeSkip ->
            offset ++ "-- Skiping Due Only tests in " ++ browserName ++ "\n" |> span skipStyle

        Skip ->
            offset ++ "-- Skipping " ++ browserName ++ "\n" |> span skipStyle

        InQueue ->
            offset ++ "..Waiting for " ++ browserName ++ "\n" |> span waitingStyle

        Running ->
            offset ++ "Running in " ++ browserName ++ "\n" |> span runningStyle


stringFromNode : Array Queue -> Node -> Int -> List Ansi.Compose.AnsiNode
stringFromNode queues node depth =
    let
        status2string_ =
            status2string depth queues

        offset =
            String.repeat depth "  "
    in
    case node of
        Text skip only text ->
            (offset ++ text ++ "\n")
                |> (if skip then
                        span skipStyle
                    else
                        span infoStyle
                   )
                |> List.singleton

        Test { status } ->
            (status |> Dict.toList |> List.map status2string_) ++ [ text "\n" ]


skipStyle : List Style
skipStyle =
    [ foreground BrightYellow ]


errorStyle : List Style
errorStyle =
    [ foreground Red ]


successStyle : List Style
successStyle =
    [ foreground Green ]


infoStyle : List Style
infoStyle =
    [ foreground BrightBlue ]


waitingStyle : List Style
waitingStyle =
    [ foreground Yellow ]


runningStyle : List Style
runningStyle =
    [ foreground BrightGreen ]


info : String -> Ansi.Compose.AnsiNode
info =
    span infoStyle
