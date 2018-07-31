module Main exposing (main)

import Ansi exposing (Color(..))
import Ansi.Compose exposing (foreground, li, renderWindow, span, text, toModel, ul)
import Ansi.Log exposing (init)
import Html exposing (pre)


main : Html.Html msg
main =
    [ data |> Html.text ]
        |> pre []


data : String
data =
    renderWindow model


model : Ansi.Log.Model
model =
    init Ansi.Log.Raw
        |> toModel
            [ [ li [] [ text "Hello" ]
              , li []
                    [ text "Hello1"
                    , span [] "On same Line1"
                    , ul []
                        [ li style [ text "last in that", ul [] [ li style [ text "deeper" ] ] ]
                        ]
                    ]
              , li [] [ text "Hello2", span [] "On same Line" ]
              , li [] [ ul [] [ li style [ text "Hello3" ] ] ]
              ]
                |> ul [ foreground Red ]
            ]


style : List Ansi.Compose.Style
style =
    [ foreground BrightBlack ]
