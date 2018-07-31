module WebDriver.Runner exposing (Configuration, TestRunner, configuration, run, runWith)

{-|

@docs run, runWith, configuration, TestRunner, Configuration

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode
import NestedSet exposing (NestedSet)
import Platform exposing (programWithFlags)
import Task exposing (Task)
import WebDriver.Internal as Internal exposing (Expectation(..), Node(..), Parsed(..), Queue(Queue), TestStatus(..), unwrap)
import WebDriver.Internal.Browser as WebDriver exposing (browser)
import WebDriver.Internal.Render exposing (render)
import WebDriver.LowLevel.Capabilities as Capabilities exposing (Capabilities)
import WebDriver.Step as WebDriver exposing (Functions)
import WebDriver.Test exposing (Test)


{-| -}
type alias TestRunner =
    Application


type alias Application =
    Platform.Program Json.Value Model Message


{-| A function which, run tests with fallback [`configuration`](#configuration),
if flags no are defined (to define own use [`runWith`](#runWith) or define with flags)
-}
run : Test -> TestRunner
run suite =
    runWith configuration suite


{-| -}
type alias Configuration =
    { dirverHost : String
    , capabilities : Json.Value
    }


{-| Default configuration that is used in [`run`](#run)
-}
configuration : Configuration
configuration =
    { dirverHost = "http://localhost:4444/wd/hub"
    , capabilities = Capabilities.encode Capabilities.default
    }


{-| Same as [`run`](#run), only allows You define Your own capabilities as raw `Json.Value`
-}
runWith : Configuration -> Test -> Application
runWith configs suite =
    programWithFlags
        { init = init configs suite
        , update = update
        , subscriptions = always Sub.none
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


update : Message -> Model -> ( Model, Cmd Message )
update (TestResult { testId, queueId } result) model =
    let
        newModel =
            { model | data = NestedSet.update testId (itemStatusDone queueId result) model.data }

        cmd =
            render newModel
    in
    case result of
        Fail True err ->
            ( newModel, cmd )

        _ ->
            let
                cmd2 =
                    nextTest queueId newModel.queues newModel.data
                        |> Maybe.map
                            (\{ caps, test, nextId } ->
                                runOne caps.dirverHost caps.capabilities test
                                    |> Task.perform (TestResult { testId = nextId, queueId = queueId })
                            )
                        |> Maybe.withDefault Cmd.none
            in
            ( newModel, Cmd.batch [ cmd, cmd2 ] )


init : Configuration -> Test -> Json.Value -> ( Model, Cmd Message )
init configs suite flags =
    let
        ({ capabilities } as configuration) =
            configurationInit configs flags

        model =
            { configuration = configuration
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
                    render resultModel
            in
            ( resultModel, [ cmd ] ++ cmds2 |> Cmd.batch )

        Error s ->
            ( model, Cmd.none )


configurationInit : Configuration -> Json.Value -> Configuration
configurationInit configs flags =
    let
        dirverHost =
            flags
                |> Json.decodeValue (Json.field "webdriverUrl" Json.string)
                |> Result.withDefault configs.dirverHost
    in
    { configs | dirverHost = dirverHost }


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
                                NestedSet.update id (itemStatusInQueue queueId) acc2
                            )
                            model
                            ids

                    ( newCmds, finalModel ) =
                        List.take caps.instances ids
                            |> List.foldl
                                (\testId ( tasks, modelWithRunning ) ->
                                    case NestedSet.get testId data of
                                        Just (Test { test, only, skip }) ->
                                            if skip then
                                                ( tasks, NestedSet.update testId (itemStatusSkip queueId) modelWithRunning )
                                            else if onlyMode && not only then
                                                ( tasks, NestedSet.update testId (itemStatusOnlyModeSkip queueId) modelWithRunning )
                                            else
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


itemStatusInQueue : Int -> Node -> Node
itemStatusInQueue queueId =
    itemStatusUpdater
        (\({ status, skip } as item) ->
            { item
                | status =
                    Dict.update queueId
                        (always
                            (if skip then
                                Just Skip
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


nextTest : Int -> Array Queue -> NestedSet Node -> Maybe { caps : Internal.BrowserData, test : Functions -> Task Never Expectation, nextId : Int }
nextTest queueId queues data =
    let
        returnIf a b =
            if b then
                Just a
            else
                Nothing

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
