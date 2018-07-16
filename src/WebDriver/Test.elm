module WebDriver.Test exposing (Functions, Test, test, describe, only, skip)

import Task exposing (Task)
import WebDriver.LowLevel.Value exposing (Answer)
import WebDriver.LowLevel.Functions as Functions
import WebDriver.Internal as Internal
import Set


type alias Test =
    Internal.Test


type alias Functions =
    Functions.Functions


only : Test -> Test
only =
    Internal.Only


skip : Test -> Test
skip =
    Internal.Skipped


test : String -> (Functions -> Task String Answer) -> Test
test untrimmedDesc thunk =
    let
        desc =
            String.trim untrimmedDesc
    in
        thunk
            >> Task.mapError (\s -> ( Internal.StopOne, s ))
            |> Internal.UnitTest
            |> Internal.Labeled desc


describe : String -> List Internal.Test -> Test
describe untrimmedDesc tests =
    let
        desc =
            String.trim untrimmedDesc
    in
        if String.isEmpty desc then
            Internal.failNow
                "This `describe` has a blank description. Let's give it a useful one!"
        else if List.isEmpty tests then
            Internal.failNow
                ("This `describe " ++ desc ++ "` has no tests in it. Let's give it some!")
        else
            case Internal.duplicatedName tests of
                Err duped ->
                    Internal.failNow
                        ("The tests '" ++ desc ++ "' contain multiple tests named '" ++ duped ++ "'. Let's rename them so we know which is which.")

                Ok childrenNames ->
                    if Set.member desc childrenNames then
                        Internal.failNow
                            ("The test '" ++ desc ++ "' contains a child test of the same name. Let's rename them so we know which is which.")
                    else
                        Internal.Labeled desc (Internal.Batch tests)
