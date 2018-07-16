module Main exposing (..)

import Task
import WebDriver.LowLevel.Value as WebDriver
import WebDriver.Test as WebDriver exposing (describe, test, skip, only)
import WebDriver.Runner as Runner exposing (run)


main =
    run suite


suite : WebDriver.Test
suite =
    describe "WebDriver"
        [ describe "LowLevel"
            [ only <|
                describe "Functions"
                    [ test "url / getUrl" <|
                        \{ url, getUrl, continue, fail } ->
                            url "https://github.com"
                                |> Task.andThen (\_ -> getUrl)
                                |> Task.andThen
                                    (\data ->
                                        case data.value of
                                            WebDriver.GetUrl string ->
                                                if string == "https://github.com/" then
                                                    continue data
                                                else
                                                    "Wrong Url - expected \"https://github.com\" but got "
                                                        ++ string
                                                        |> fail

                                            _ ->
                                                fail "Wrong Url"
                                    )

                    -- , test "Level5-test" <|
                    --     \{ url } ->
                    --         url "https://github.com"
                    --             -- |> Task.andThen (\data -> Task.fail "3")
                    --             |> Task.andThen (\_ -> url "https://github.com/justgook")
                    --             |> Task.andThen (\_ -> url "https://github.com/justgook/elm-tiled-decode")
                    ]
            , describe "Level6"
                [ test "Level7-test" <|
                    \{ url } ->
                        url "https://github.com"
                            -- |> Task.andThen (\data -> Task.fail "3")
                            |> Task.andThen (\_ -> url "https://github.com/justgook")
                            |> Task.andThen (\_ -> url "https://github.com/justgook/elm-tiled-decode")
                ]
            ]
        , describe "only-describe"
            [ test "OnlyTest" <| \{ url } -> url "https://github.com"
            , test "OnlyTest2" <| \{ url } -> url "https://github.com"
            ]
        , skip <| test "Last for skip" <| \{ url } -> url "https://github.com"
        ]
