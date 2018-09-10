module Test.Action exposing (tests)

import Json.Decode as Decode
import Json.Encode as Json
import Task
import WebDriver as WebDriver exposing (describe, only, test)
import WebDriver.Assert as Assert
import WebDriver.Step.Action as Action


hostForCookies : String
hostForCookies =
    "http://example.com"


tests : List (WebDriver.Test a)
tests =
    [ describe "actions"
        (List.map
            (\( name, action ) ->
                describe name
                    [ test "down" <|
                        \{ url, actions } ->
                            url blankPage |> Task.andThen (\_ -> actions [ action ("_" ++ name ++ "Id") (\{ down } -> [ down 0 ]) ])
                    , test "up" <|
                        \{ url, actions } ->
                            url blankPage |> Task.andThen (\_ -> actions [ action ("_" ++ name ++ "Id") (\{ up } -> [ up 0 ]) ])
                    , test "move" <|
                        \{ url, actions } ->
                            url blankPage |> Task.andThen (\_ -> actions [ action ("_" ++ name ++ "Id") (\{ move } -> [ move { duration = 10, origin = Action.OriginPointer, x = 50, y = 50 } ]) ])
                    , test "pause" <|
                        \{ url, actions } ->
                            url blankPage |> Task.andThen (\_ -> actions [ action ("_" ++ name ++ "Id") (\{ pause } -> [ pause 10 ]) ])
                    ]
            )
            [ ( "mouse", Action.mouse )
            , ( "pen", Action.pen )
            , ( "touch", Action.touch )
            ]
        )
    , describe "keyboard"
        [ test "up" <|
            \{ url, actions } ->
                url blankPage |> Task.andThen (\_ -> actions [ Action.key "_keyboardId" (\{ up } -> [ up 'o' ]) ])
        , test "down" <|
            \{ url, actions } ->
                url blankPage |> Task.andThen (\_ -> actions [ Action.key "_keyboardId" (\{ down } -> [ down 'o' ]) ])
        , test "pause" <|
            \{ url, actions } ->
                url blankPage |> Task.andThen (\_ -> actions [ Action.key "_keyboardId" (\{ pause } -> [ pause 10 ]) ])
        ]
    , test "release" <|
        \{ url, release } -> url blankPage |> (\_ -> release)
    ]


blankPage : String
blankPage =
    """data:text/html,
    <h1>Hello World</h1>"""
