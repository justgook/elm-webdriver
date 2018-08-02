module WebDriver.Internal.Render exposing (renderDot, renderDotLive, renderSpec, renderSpecLive)

import Ansi exposing (Color(..))
import Ansi.Compose exposing (Style, div, foreground, renderWindow, span, text, toModel)
import Ansi.Log exposing (init)
import Array exposing (Array)
import Dict
import NestedSet exposing (NestedSet)
import WebDriver.Internal exposing (Expectation(..), Node(..), Queue(..), TestStatus(..))
import WebDriver.Internal.Port as Log


-- http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html


renderDot : { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> Cmd msg
renderDot input =
    Log.log " - Not Implemented Yet"


renderDotLive : { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> Cmd msg
renderDotLive ({ queues, data, onlyMode } as input) =
    let
        render_ index node acc =
            let
                nextNodes =
                    case node of
                        Test { status, only } ->
                            status |> Dict.toList |> List.map status2dot

                        Text _ _ _ ->
                            []
            in
            acc ++ nextNodes

        result =
            init Ansi.Log.Raw
                |> toModel (NestedSet.indexedFoldr render_ [] data)
                |> renderWindow
    in
    "\x1B[1000D\x1B[2K\x1B[1A" ++ result ++ "\n" |> Log.log


renderSpec : { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> Cmd msg
renderSpec input =
    Log.log " - Not Implemented Yet"


renderSpecLive : { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> Cmd msg
renderSpecLive ({ queues, data, onlyMode } as input) =
    let
        render_ index node acc =
            let
                depth =
                    NestedSet.depth index data

                nextNodes =
                    stringFromNode onlyMode queues node depth
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


stringFromNode : Bool -> Array Queue -> Node -> Int -> List Ansi.Compose.AnsiNode
stringFromNode onlyMode queues node depth =
    let
        status2string_ =
            status2string depth queues

        offset =
            String.repeat depth "  "
    in
    case node of
        Text skip only text ->
            if onlyMode && not only then
                []
            else
                (offset ++ text ++ "\n")
                    |> (if skip then
                            span skipStyle
                        else
                            span infoStyle
                       )
                    |> List.singleton

        Test { status, only } ->
            if onlyMode && not only then
                []
            else
                (status |> Dict.toList |> List.map status2string_) ++ [ text "\n" ]


status2string : Int -> Array Queue -> ( Int, TestStatus ) -> Ansi.Compose.AnsiNode
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


status2dot : ( a, TestStatus ) -> Ansi.Compose.AnsiNode
status2dot ( queueID, status ) =
    case status of
        Done (Fail critical {- Exit -} error) ->
            "F" |> span errorStyle

        Done Pass ->
            "." |> span successStyle

        OnlyModeSkip ->
            "-" |> span skipStyle

        Skip ->
            "-" |> span skipStyle

        InQueue ->
            "_" |> span waitingStyle

        Running ->
            "R" |> span runningStyle


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
