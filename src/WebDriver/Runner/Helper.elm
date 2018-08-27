module WebDriver.Runner.Helper exposing (addParralel, collectBy, countBy, init, mapExit, next)

import Array exposing (Array)
import NestedSet
import WebDriver.Runner as Runner


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


next mapTest expectation queueId testId model =
    let
        newModel =
            Runner.updateDone expectation queueId testId model
    in
    nextIntenal mapTest queueId newModel


nextIntenal mapTest queueId newModel =
    Runner.prepareNext queueId newModel
        |> Maybe.map (\( m, c ) -> ( m, mapTest queueId c ))
        |> Maybe.withDefault ( newModel, Cmd.none )


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
