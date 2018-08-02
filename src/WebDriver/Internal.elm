module WebDriver.Internal
    exposing
        ( BrowserData
        , ErrorLevel(..)
        , Expectation(..)
        , Node(..)
        , Parsed(..)
        , Queue(..)
        , Test(..)
        , TestStatus(..)
        , duplicatedName
        , failNow
        , unwrap
        )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as Json
import NestedSet exposing (NestedSet)
import Set exposing (Set)
import Task exposing (Task)
import WebDriver.Step exposing (Functions)


type Test
    = UnitTest (Functions -> Task Never Expectation)
    | Browser (List BrowserData) Test
      -- | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Skipped Test
    | Only Test
    | Batch (List Test)
    | ParseError String


type alias BrowserData =
    { name : String
    , instances : Int
    , dirverHost : String
    , capabilities : Json.Value
    }


type ErrorLevel
    = StopOne
    | StopAll


type Expectation
    = Pass
    | Fail Bool {- Exit -} String


failNow : String -> Test
failNow description =
    ParseError description


duplicatedName : List Test -> Result String (Set String)
duplicatedName =
    let
        names : Test -> List String
        names test =
            case test of
                Labeled str _ ->
                    [ str ]

                Batch subtests ->
                    List.concatMap names subtests

                UnitTest _ ->
                    []

                ParseError _ ->
                    []

                Browser _ subTest ->
                    names subTest

                -- FuzzTest _ ->
                -- []
                Skipped subTest ->
                    names subTest

                Only subTest ->
                    names subTest

        insertOrFail : String -> Result String (Set String) -> Result String (Set String)
        insertOrFail newName =
            Result.andThen
                (\oldNames ->
                    if Set.member newName oldNames then
                        Err newName
                    else
                        Ok <| Set.insert newName oldNames
                )
    in
    List.concatMap names
        >> List.foldl insertOrFail (Ok Set.empty)


type Parsed
    = Success { onlyMode : Bool, data : NestedSet Node, queues : Array Queue }
    | Error String


type Node
    = Text Bool Bool String
    | Test
        { skip : Bool
        , only : Bool
        , test : Functions -> Task Never Expectation
        , status : Dict Int TestStatus
        }


type TestStatus
    = Done Expectation
    | Skip
    | InQueue
    | Running
    | OnlyModeSkip


type Queue
    = Queue ( BrowserData, List Int )


unwrap : List BrowserData -> Test -> Parsed
unwrap capabilities current =
    let
        configs =
            { capabilities = capabilities
            , skip = False
            , only = False
            }

        model =
            ( [], Success { onlyMode = False, data = NestedSet.empty, queues = Array.empty } )
    in
    case unwrapFold current -1 configs model of
        ( queue, Success result ) ->
            let
                newQueues =
                    List.map (\caps -> Queue ( caps, queue )) capabilities
                        |> Array.fromList
                        |> Array.append result.queues
            in
            Success { result | queues = newQueues }

        ( _, Error err ) ->
            Error err


unwrapFold : Test -> Int -> { a | only : Bool, skip : Bool } -> ( List Int, Parsed ) -> ( List Int, Parsed )
unwrapFold branch parentId ({ skip, only } as configs) ( queue, acc ) =
    case ( branch, acc ) of
        ( _, Error a ) ->
            ( queue, acc )

        ( UnitTest test, Success ({ data } as model) ) ->
            let
                item =
                    Test
                        { skip = skip
                        , only = only
                        , test = test
                        , status = Dict.empty
                        }

                itemId =
                    NestedSet.length data

                newData =
                    NestedSet.insert parentId item data
            in
            ( queue ++ [ itemId ]
            , Success { model | data = NestedSet.insert parentId item data }
            )

        ( Browser caps rest, Success { onlyMode, data } ) ->
            case unwrapFold rest parentId configs ( [], acc ) of
                ( subQueue, Success result ) ->
                    let
                        newQueues =
                            List.map (\caps -> Queue ( caps, subQueue )) caps
                                |> Array.fromList
                                |> Array.append result.queues
                    in
                    ( queue, Success { result | queues = newQueues } )

                ( _, Error err ) ->
                    ( [], Error err )

        ( Labeled text rest, Success ({ onlyMode, data } as model) ) ->
            let
                item =
                    Text skip only text

                itemId =
                    NestedSet.length data

                newAcc =
                    Success { model | data = NestedSet.insert parentId item data }
            in
            unwrapFold rest itemId configs ( queue, newAcc )

        ( Skipped rest, Success { onlyMode, data } ) ->
            unwrapFold rest parentId { configs | skip = True } ( queue, acc )

        ( Only rest, Success model ) ->
            unwrapFold rest parentId { configs | only = True } ( queue, Success { model | onlyMode = True } )

        ( Batch listRest, Success { onlyMode, data } ) ->
            listRest
                |> List.foldl (\rest acc2 -> unwrapFold rest parentId configs acc2) ( queue, acc )

        ( ParseError err, Success { onlyMode, data } ) ->
            ( queue, Error err )
