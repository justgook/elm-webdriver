module WebDriver.Runner exposing (run, runWith, configuration, TestRunner, Configuration, Reporter(..))

{-|

@docs run, runWith, configuration, TestRunner, Configuration, Reporter

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode
import NestedSet exposing (NestedSet)
import Platform exposing (worker)
import Task exposing (Task)
import WebDriver exposing (Test)
import WebDriver.Helper.Capabilities as Capabilities exposing (Capabilities)
import WebDriver.Internal as Internal exposing (Expectation(..), Node(..), Parsed(..), Queue(..), TestStatus(..), unwrap)
import WebDriver.Internal.Browser as WebDriver exposing (browser)
import WebDriver.Internal.Render as Render
import WebDriver.Step as WebDriver exposing (Functions)


{-| Platform.Program that can be runned as worker or cli application
-}
type alias TestRunner =
    PlatformProgramWithFlags


{-| How to output results, results will be pushed to `log` port as string,

overriding with flag `reporter`

  - `dot-live` - dot notation (each test is represented as `.` ) and updates status in live mode
  - `dot` - same as do `dot-live` only for continuous integration servers, **not** updates status in live mode
  - `spec-live` - print out all texts of tests / describe in nested list way and updates status in live mode
  - `spec`- same as do `spec-live` only for continuous integration servers, **not** updates status in live mode

-}
type Reporter
    = DotReporter
    | DotLiveReporter
    | SpecReporter
    | SpecLiveReporter


type alias PlatformProgramWithFlags =
    Platform.Program Json.Value Model Message


{-| A function which, run tests with fallback [`configuration`](#configuration),
if flags no are defined (to define own use [`runWith`](#runWith) or define with flags)
-}
run : Test -> TestRunner
run suite =
    runWith configuration suite


{-| Same as [`run`](#run),
only allows You define Your own

  - `capabilities` - `Json.Value` - [more info](https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities)
  - `host` - `String` - WebDriver host
  - `reporter` - [`Reporter`](#Reporter)

all that data can be overriding with corresponding flags

-}
runWith : Configuration -> Test -> PlatformProgramWithFlags
runWith configs suite =
    worker
        { init = init configs suite
        , update = update
        , subscriptions = always Sub.none
        }


{-| Configuration for [`runWith`](#runWith)
-}
type alias Configuration =
    { dirverHost : String
    , capabilities : Json.Value
    , reporter : Reporter
    }


{-| Default configuration that is used in [`run`](#run)

  - `host` - `http://localhost:4444/wd/hub` - Default for [Selenium Standalone Server](https://www.seleniumhq.org/download/)
  - `capabilities` - plain Chrome
  - [`reporter`](#Reporter) - `SpecLiveReporter`

-}
configuration : Configuration
configuration =
    { dirverHost = "http://localhost:4444/wd/hub"
    , capabilities = Capabilities.encode Capabilities.default
    , reporter = SpecReporter
    }


type alias Model =
    { configuration : Configuration
    , data : NestedSet Node
    , onlyMode : Bool
    , queues : Array Queue
    }


type Message
    = TestResult { testId : Int, queueId : Int } Expectation


type alias TestCoordinate =
    { testId : Int, queueId : Int }


render : Reporter -> { a | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> Render.LastResult -> Cmd msg
render reporter =
    case reporter of
        DotReporter ->
            Render.renderDot

        DotLiveReporter ->
            -- Render.renderDotLive
            Render.renderDot

        SpecReporter ->
            -- Render.renderSpec
            Render.renderDot

        SpecLiveReporter ->
            -- Render.renderSpecLive
            Render.renderDot


update : Message -> Model -> ( Model, Cmd Message )
update (TestResult { testId, queueId } result) model =
    let
        newModel =
            { model | data = NestedSet.update testId (itemStatusDone queueId result) model.data }

        status =
            if result == Pass then
                Render.Good

            else
                Render.Bad

        renderCmd =
            render model.configuration.reporter newModel status
    in
    case result of
        Fail True err ->
            ( newModel, renderCmd )

        _ ->
            nextTest queueId newModel.queues newModel.data
                |> Maybe.map
                    (\{ caps, test, nextId } ->
                        ( { newModel
                            | data = NestedSet.update nextId (itemStatusRunning queueId) newModel.data
                          }
                        , runOne caps.dirverHost caps.capabilities test
                            |> Task.perform (TestResult { testId = nextId, queueId = queueId })
                        )
                    )
                |> Maybe.withDefault ( newModel, Cmd.none )
                |> addCmd renderCmd


init : Configuration -> Test -> Json.Value -> ( Model, Cmd Message )
init configs suite flags =
    let
        ({ capabilities } as newConfig) =
            configurationInit configs flags

        model =
            { configuration = newConfig
            , data = NestedSet.empty
            , onlyMode = False
            , queues = Array.empty
            }

        browserData =
            { name = "Chrome"
            , instances = 8
            , capabilities = capabilities
            , dirverHost = configs.dirverHost
            }
    in
    case unwrap [ browserData ] suite of
        Success { onlyMode, data, queues } ->
            let
                initialModel =
                    { model
                        | data = data
                        , queues = queues
                        , onlyMode = onlyMode
                    }

                ( cmds2, newData ) =
                    initRun initialModel

                resultModel =
                    { initialModel | data = newData }

                cmd =
                    render model.configuration.reporter resultModel Render.Init
            in
            ( resultModel, [ cmd ] ++ cmds2 |> Cmd.batch )

        Error s ->
            ( model, Cmd.none )


configurationInit : Configuration -> Json.Value -> Configuration
configurationInit configs flags =
    let
        dirverHost =
            flags
                |> Json.decodeValue (Json.field "host" Json.string)
                |> Result.withDefault configs.dirverHost

        reporter =
            flags
                |> Json.decodeValue
                    (Json.field "reporter" Json.string
                        |> Json.map
                            (\value ->
                                case value of
                                    "dot" ->
                                        DotReporter

                                    "dot-live" ->
                                        DotLiveReporter

                                    "spec" ->
                                        SpecReporter

                                    "spec-live" ->
                                        SpecLiveReporter

                                    _ ->
                                        configs.reporter
                            )
                    )
                |> Result.withDefault configs.reporter

        capabilities =
            flags
                |> Json.decodeValue (Json.field "capabilities" Json.value)
                |> Result.withDefault configs.capabilities
    in
    { capabilities = capabilities
    , dirverHost = dirverHost
    , reporter = reporter
    }


availableToRun : NestedSet Node -> Int -> Int -> Bool
availableToRun data queueId itemId =
    NestedSet.get itemId data
        |> Maybe.andThen
            (\item ->
                case item of
                    Test { status, test } ->
                        Dict.get queueId status
                            |> Maybe.map ((==) InQueue)

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault False


initRun : { b | data : NestedSet Node, onlyMode : Bool, queues : Array Queue } -> ( List (Cmd Message), NestedSet Node )
initRun { data, queues, onlyMode } =
    queues
        |> Array.toIndexedList
        |> List.foldl
            (\( queueId, Queue ( caps, ids ) ) ( acc, model ) ->
                let
                    modelWithQueue =
                        List.foldl
                            (\id acc2 ->
                                NestedSet.update id (initItemQueueStatus onlyMode queueId) acc2
                            )
                            model
                            ids

                    ( newCmds, finalModel ) =
                        ids
                            |> List.filter (availableToRun modelWithQueue queueId)
                            |> List.take caps.instances
                            |> List.foldl
                                (\testId ( tasks, modelWithRunning ) ->
                                    case NestedSet.get testId modelWithRunning of
                                        Just (Test { test, only, skip }) ->
                                            ( (runOne caps.dirverHost caps.capabilities test
                                                |> Task.perform (TestResult { testId = testId, queueId = queueId })
                                              )
                                                :: tasks
                                            , NestedSet.update testId (itemStatusRunning queueId) modelWithRunning
                                            )

                                        _ ->
                                            -- It should never happent - but have no clue how to make it type safe
                                            ( tasks, modelWithRunning )
                                )
                                ( [], modelWithQueue )
                in
                ( acc ++ newCmds, finalModel )
            )
            ( [], data )


itemStatusUpdater : ({ only : Bool, skip : Bool, test : Functions -> Task Never Expectation, status : Dict Int TestStatus } -> { only : Bool, skip : Bool, status : Dict Int TestStatus, test : Functions -> Task Never Expectation }) -> Node -> Node
itemStatusUpdater updater item =
    case item of
        Test data ->
            Test (updater data)

        Text a b c ->
            Text a b c


itemStatusUpdater2 : TestStatus -> Int -> Node -> Node
itemStatusUpdater2 status queueId item =
    case item of
        Test data ->
            Test ((\a -> { a | status = Dict.update queueId (always (Just status)) a.status }) data)

        Text a b c ->
            Text a b c


initItemQueueStatus : Bool -> Int -> Node -> Node
initItemQueueStatus onlyMode queueId =
    itemStatusUpdater
        (\({ status, skip, only } as item) ->
            { item
                | status =
                    Dict.update queueId
                        (always
                            (if skip then
                                Just Skip

                             else if onlyMode && not only then
                                Just OnlyModeSkip

                             else
                                Just InQueue
                            )
                        )
                        status
            }
        )


itemStatusRunning : Int -> Node -> Node
itemStatusRunning =
    itemStatusUpdater2 Running


itemStatusSkip : Int -> Node -> Node
itemStatusSkip =
    itemStatusUpdater2 Skip


itemStatusOnlyModeSkip : Int -> Node -> Node
itemStatusOnlyModeSkip =
    itemStatusUpdater2 OnlyModeSkip


itemStatusDone : Int -> Expectation -> Node -> Node
itemStatusDone queueId result =
    itemStatusUpdater
        (\a ->
            { a
                | status =
                    Dict.update queueId (always (Just (Done result))) a.status
            }
        )


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model, Cmd.batch [ cmd, oldCmd ] )


returnIf : a -> Bool -> Maybe a
returnIf a b =
    if b then
        Just a

    else
        Nothing


nextTest : Int -> Array Queue -> NestedSet Node -> Maybe { caps : Internal.BrowserData, test : Functions -> Task Never Expectation, nextId : Int }
nextTest queueId queues data =
    let
        validateId browserData id =
            NestedSet.get id data
                |> Maybe.andThen
                    (\item ->
                        case item of
                            Test { status, test } ->
                                Dict.get queueId status
                                    |> Maybe.andThen ((==) InQueue >> returnIf { caps = browserData, test = test, nextId = id })

                            _ ->
                                Nothing
                    )

        searchForNext browserData ids =
            case ids of
                [] ->
                    Nothing

                id :: [] ->
                    validateId browserData id

                id :: rest ->
                    case validateId browserData id of
                        Nothing ->
                            searchForNext browserData rest

                        result ->
                            result
    in
    case Array.get queueId queues of
        Just (Queue ( browserData, ids )) ->
            searchForNext browserData ids

        Nothing ->
            Nothing


runOne : String -> Json.Encode.Value -> (Functions -> Task Never Expectation) -> Task Never Expectation
runOne driverUrl capabilities test =
    WebDriver.browser driverUrl capabilities test
