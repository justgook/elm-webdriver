module WebDriver.Internal exposing (Test(..), UnitTestFunction, duplicatedName, failNow)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as Json
import Set exposing (Set)
import Task exposing (Task)
import WebDriver.Step exposing (Functions)


type alias UnitTestFunction =
    Functions -> Task Never (Result String ())


type Test browserInfo
    = UnitTest UnitTestFunction
    | Browser (List browserInfo) (Test browserInfo)
      -- | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String (Test browserInfo)
    | Skipped (Test browserInfo)
    | Only (Test browserInfo)
    | Batch (List (Test browserInfo))
    | ParseErr String


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
