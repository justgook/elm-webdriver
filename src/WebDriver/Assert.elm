module WebDriver.Assert exposing
    ( custom, fail
    , equal, equalInt, true, false, atLeast, greaterThan
    )

{-|

@docs custom, fail
@docs equal, equalInt, true, false, atLeast, greaterThan

-}

import Task exposing (Task)


{-| Checks if both values is same, and if not fails with fail function
-}
custom : (a -> String) -> a -> a -> Task String a
custom f a b =
    if a == b then
        Task.succeed b

    else
        Task.fail (f b)


{-| Passes if the arguments is same strings.
-}
equal : String -> String -> Task String String
equal a b =
    custom (\_ -> verticalBar "Assert.equal" a b) a b


{-| Passes if the arguments is same Int.
-}
equalInt : Int -> Int -> Task String Int
equalInt a b =
    custom (\_ -> verticalBar "Assert.equalInt" (String.fromInt a) (String.fromInt b)) a b


{-| Passes if the argument is 'True', and otherwise fails with the given message.
-}
true : String -> Bool -> Task String Bool
true t =
    custom (always t) True


{-| Passes if the argument is 'False', and otherwise fails with the given message.
-}
false : String -> Bool -> Task String Bool
false t =
    custom (always t) False


{-| Passes if the second argument is greater than or equal to the first.
-}
atLeast : Int -> Int -> Task String Int
atLeast a b =
    if a <= b then
        Task.succeed b

    else
        verticalBar "Assert.atLeast" (String.fromInt a) (String.fromInt b)
            |> Task.fail


{-| Passes if the second argument is greater than the first.
-}
greaterThan : Int -> Int -> Task String Int
greaterThan a b =
    if a < b then
        Task.succeed b

    else
        verticalBar "Assert.greaterThan" (String.fromInt a) (String.fromInt b)
            |> Task.fail


{-| Throws an Assertion Error
-}
fail : a -> e -> Task e b
fail _ e =
    Task.fail e


verticalBar : String -> String -> String -> String
verticalBar comparison expected actual =
    [ expected
    , "╵"
    , "│ " ++ comparison
    , "╷"
    , actual
    ]
        |> String.join "\n"
