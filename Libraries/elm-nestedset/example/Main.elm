module Main exposing (main)

import Html exposing (pre, text)
import NestedSet exposing (NestedSet)


main : Html.Html msg
main =
    [ data |> text ]
        |> pre []


data : String
data =
    let
        result =
            NestedSet.empty
                |> NestedSet.insert -1 "1.top-level"
                |> NestedSet.insert -1 "2.top-level"
                |> NestedSet.insert 1 "2.1"
                |> NestedSet.insert 0 "1.1"
                |> NestedSet.insert 0 "1.2"
                |> NestedSet.insert 0 "1.3"
                |> NestedSet.insert 1 "2.2"
                |> NestedSet.insert 1 "2.3"
                |> NestedSet.insert 0 "1.4"
                |> NestedSet.insert 8 "1.4.1"
                |> NestedSet.insert -1 "3.top-level"
    in
    stringify identity result
        ++ "\n\n+++++++ Reordered +++++++\n\n"
        ++ stringify identity (NestedSet.order result)


stringify : (a -> String) -> NestedSet a -> String
stringify getString tree =
    let
        render index data acc =
            acc ++ String.repeat (NestedSet.depth index tree) " " ++ getString data ++ " (id: " ++ toString index ++ ")" ++ "\n"
    in
    NestedSet.indexedFoldr render "" tree
