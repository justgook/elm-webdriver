module Main exposing (all)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import NestedSet
import Test exposing (Test, describe, fuzz, test)


all : Test
all =
    describe "Nested Set Model"
        [ describe "Creating"
            [ test "empti" <|
                \_ ->
                    NestedSet.empty
                        |> NestedSet.length
                        |> Expect.equal 0
            ]
        , describe "Inserting"
            [ fuzz (list string) "insert all in top level" <|
                \items ->
                    List.foldr
                        (\i acc -> NestedSet.insert -1 i acc)
                        (NestedSet.insert -1 "first" NestedSet.empty)
                        items
                        |> NestedSet.length
                        |> Expect.equal (List.length items + 1)
            , fuzz (list string) "insert one each other" <|
                \items ->
                    List.foldr
                        (\i acc -> NestedSet.insert (NestedSet.length acc - 1) i acc)
                        (NestedSet.insert -1 "first" NestedSet.empty)
                        items
                        |> NestedSet.length
                        |> Expect.equal (List.length items + 1)
            , fuzz (list string) "insert few elemnts to parent node" <|
                \items ->
                    List.foldr
                        (\i acc ->
                            acc
                                |> NestedSet.insert (NestedSet.length acc - 1) i
                                |> NestedSet.insert (NestedSet.length acc - 1) i
                                |> NestedSet.insert (NestedSet.length acc - 1) i
                                |> NestedSet.insert (NestedSet.length acc - 1) i
                        )
                        (NestedSet.insert -1 "first" NestedSet.empty)
                        items
                        |> NestedSet.length
                        |> Expect.equal (List.length items * 4 + 1)
            , fuzz (list string) "insertAfter" <|
                \items ->
                    List.foldr
                        (\i acc ->
                            NestedSet.insertAfter (NestedSet.length acc - 1) i acc
                        )
                        (NestedSet.insert -1 "first" NestedSet.empty)
                        items
                        |> NestedSet.length
                        |> Expect.equal (List.length items + 1)
            ]
        ]
