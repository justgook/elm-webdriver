module WebDriver.Internal exposing (BrowserData, ErrLevel(..), Expectation, Node(..), Queue(..), Test(..), TestStatus(..), duplicatedName, failNow)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as Json
import NestedSet exposing (NestedSet)
import Set exposing (Set)
import Task exposing (Task)
import WebDriver.Step exposing (Functions)


type Test browserInfo
    = UnitTest (Functions -> Task Never (Result String ()))
    | Browser (List browserInfo) (Test browserInfo)
      -- | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String (Test browserInfo)
    | Skipped (Test browserInfo)
    | Only (Test browserInfo)
    | Batch (List (Test browserInfo))
    | ParseErr String


type alias BrowserData =
    { -- name : String
      -- , instances : Int
      -- , dirverHost : String
      -- ,
      url : String
    , capabilities : Json.Value
    }


type ErrLevel
    = StopOne
    | StopAll


type alias Expectation =
    Result { critical : Bool, error : String, capabilities : Json.Value } Json.Value


failNow : String -> Test browserInfo
failNow description =
    ParseErr description


duplicatedName : List (Test browserInfo) -> Result String (Set String)
duplicatedName =
    let
        names : Test browserInfo -> List String
        names test =
            case test of
                Labeled str _ ->
                    [ str ]

                Batch subtests ->
                    List.concatMap names subtests

                UnitTest _ ->
                    []

                ParseErr _ ->
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


type alias Parsed =
    Result String { onlyMode : Bool, data : NestedSet Node, queues : Array Queue }


type Node
    = Text Bool Bool String
    | Test
        { skip : Bool
        , only : Bool
        , test : Functions -> Task Never (Result String ())
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



-- unwrap : List BrowserData -> Test -> Result String { onlyMode : Bool, data : NestedSet Node, queues : Array Queue }
-- unwrap capabilities current =
--     let
--         configs =
--             { capabilities = capabilities
--             , skip = False
--             , only = False
--             }
--         model =
--             ( [], Ok { onlyMode = False, data = NestedSet.empty, queues = Array.empty } )
--     in
--     case unwrapFold current -1 configs model of
--         ( queue, Ok result ) ->
--             let
--                 newQueues =
--                     List.map (\caps -> Queue ( caps, queue )) capabilities
--                         |> Array.fromList
--                         |> Array.append result.queues
--             in
--             Ok { result | queues = newQueues }
--         ( _, Err err ) ->
--             Err err
-- unwrapFold : Test -> Int -> { a | only : Bool, skip : Bool } -> ( List Int, Parsed ) -> ( List Int, Parsed )
-- unwrapFold branch parentId ({ skip, only } as configs) ( queue, acc ) =
--     case ( branch, acc ) of
--         ( _, Err a ) ->
--             ( queue, acc )
--         ( UnitTest test, Ok ({ data } as model) ) ->
--             let
--                 item =
--                     Test
--                         { skip = skip
--                         , only = only
--                         , test = test
--                         , status = Dict.empty
--                         }
--                 itemId =
--                     NestedSet.length data
--                 newData =
--                     NestedSet.insert parentId item data
--             in
--             ( queue ++ [ itemId ]
--             , Ok { model | data = NestedSet.insert parentId item data }
--             )
--         ( Browser caps rest, Ok { onlyMode, data } ) ->
--             case unwrapFold rest parentId configs ( [], acc ) of
--                 ( subQueue, Ok result ) ->
--                     let
--                         newQueues =
--                             List.map (\caps_ -> Queue ( caps_, subQueue )) caps
--                                 |> Array.fromList
--                                 |> Array.append result.queues
--                     in
--                     ( queue, Ok { result | queues = newQueues } )
--                 ( _, Err err ) ->
--                     ( [], Err err )
--         ( Labeled text rest, Ok ({ onlyMode, data } as model) ) ->
--             let
--                 item =
--                     Text skip only text
--                 itemId =
--                     NestedSet.length data
--                 newAcc =
--                     Ok { model | data = NestedSet.insert parentId item data }
--             in
--             unwrapFold rest itemId configs ( queue, newAcc )
--         ( Skipped rest, Ok { onlyMode, data } ) ->
--             unwrapFold rest parentId { configs | skip = True } ( queue, acc )
--         ( Only rest, Ok model ) ->
--             unwrapFold rest parentId { configs | only = True } ( queue, Ok { model | onlyMode = True } )
--         ( Batch listRest, Ok { onlyMode, data } ) ->
--             listRest
--                 |> List.foldl (\rest acc2 -> unwrapFold rest parentId configs acc2) ( queue, acc )
--         ( ParseErr err, Ok { onlyMode, data } ) ->
--             ( queue, Err err )
