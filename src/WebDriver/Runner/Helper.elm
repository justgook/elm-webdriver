module WebDriver.Runner.Helper exposing (init, addParralel, next, collectBy, countBy, mapExit)

{-|

@docs init, addParralel, next, collectBy, countBy, mapExit

-}

import Array exposing (Array)
import NestedSet exposing (NestedSet)
import WebDriver exposing (Test)
import WebDriver.Runner as Runner exposing (Browsers, Executable, Expectation, Status)


{-| -}
init :
    Test info
    -> info
    ->
        (Int
         -> { exec : Executable, info : info, testId : Int }
         -> Cmd msg
        )
    -> (String -> Cmd msg)
    -> ( ( NestedSet String, Browsers info ), List (Cmd msg) )
init suite config mapTest mapParseError =
    case Runner.prepare config suite of
        Ok (( description, { tests } ) as partsed) ->
            tests
                |> Array.toIndexedList
                |> List.foldr
                    (\( queueId, _ ) ( newModel, cmds_ ) ->
                        Runner.prepareNext queueId newModel
                            |> Maybe.map (\( m, c ) -> ( m, [ mapTest queueId c ] |> (++) cmds_ ))
                            |> Maybe.withDefault ( newModel, cmds_ )
                    )
                    ( partsed, [] )

        Err e ->
            ( ( NestedSet.empty, { onlyMode = False, tests = Array.empty } ), [ mapParseError e ] )


{-| -}
addParralel :
    Int
    -> Int
    ->
        (Int
         -> { exec : Executable, info : info, testId : Int }
         -> Cmd msg
        )
    -> ( ( NestedSet String, Browsers info ), List (Cmd msg) )
    -> ( ( NestedSet String, Browsers info ), List (Cmd msg) )
addParralel queueId count mapTest model =
    let
        appender f count_ m_ =
            if count_ > 0 then
                appender f (count_ - 1) (f m_)

            else
                m_
    in
    appender
        (\( m, t ) ->
            nextIntenal mapTest queueId m
                |> Tuple.mapSecond (\a -> a :: t)
        )
        count
        model


{-| -}
countBy : ({ descId : Int, exec : Executable, info : info, status : Status } -> Bool) -> ( NestedSet String, Browsers info ) -> Int
countBy filter model =
    model
        |> Tuple.second
        |> .tests
        |> Array.foldr
            (\testList acc ->
                testList
                    |> List.filter filter
                    |> List.length
                    |> (+) acc
            )
            0


{-| -}
collectBy : ({ descId : Int, exec : Executable, info : info, status : Status } -> Bool) -> ( NestedSet String, Browsers info ) -> List { descId : Int, exec : Executable, info : info, status : Status }
collectBy filter model =
    model
        |> Tuple.second
        |> .tests
        |> Array.foldr
            (\testList acc ->
                testList
                    |> List.filter filter
                    |> (++) acc
            )
            []


{-| -}
next :
    (Int
     -> { exec : Executable, info : info, testId : Int }
     -> Cmd msg
    )
    -> Expectation
    -> Int
    -> Int
    -> ( NestedSet String, Browsers info )
    -> ( ( NestedSet String, Browsers info ), Cmd msg )
next mapTest expectation queueId testId model =
    let
        newModel =
            Runner.updateDone expectation queueId testId model
    in
    nextIntenal mapTest queueId newModel


nextIntenal :
    (Int
     -> { exec : Executable, info : info, testId : Int }
     -> Cmd msg
    )
    -> Int
    -> ( NestedSet String, Browsers info )
    -> ( ( NestedSet String, Browsers info ), Cmd msg )
nextIntenal mapTest queueId newModel =
    Runner.prepareNext queueId newModel
        |> Maybe.map (\( m, c ) -> ( m, mapTest queueId c ))
        |> Maybe.withDefault ( newModel, Cmd.none )


{-| -}
mapExit : (( NestedSet String, Browsers info ) -> a) -> Expectation -> ( ( NestedSet String, Browsers info ), a ) -> ( ( NestedSet String, Browsers info ), a )
mapExit report expectation (( newModel, _ ) as goForward) =
    case ( Runner.allDone newModel, expectation ) of
        ( False, Ok _ ) ->
            goForward

        ( False, Err { critical } ) ->
            if critical then
                ( newModel, report newModel )

            else
                goForward

        ( True, _ ) ->
            ( newModel, report newModel )
