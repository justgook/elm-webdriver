module WebDriver.Setup exposing
    ( setup, next, Next(..), Return, Reference, SuiteState, Validator
    , Report, Status(..)
    , isFail
    )

{-|


## Runing

@docs setup, next, Next, Return, Reference, SuiteState, Validator


## Report

@docs Report, Status


## helper

@docs isFail

-}

import Array exposing (Array)
import Json.Encode as E
import Task exposing (Task)
import Util.List.Extra as List exposing (mapAccuml, mapAccumr)
import WebDriver exposing (Test)
import WebDriver.Internal exposing (Test(..), UnitTestFunction)
import WebDriver.Internal.Browser exposing (Expectation, browser)


{-| -}
type Next info
    = Report (Report info)
    | Tasks (Tasks info)


{-| -}
type alias InfoData a =
    { a
        | url : String
        , capabilities : E.Value
    }


{-| -}
type Status
    = Pass
    | Fail String
    | Skip


{-| -}
type alias Report info =
    List
        { desc : List String
        , info : info
        , status : Status
        }


{-| -}
type RunStatus
    = Done Expectation
    | Waiting UnitTestFunction
    | Running
    | RunTimeSkip


{-| -}
type alias Item info =
    { status : RunStatus
    , info : info
    , desc : List String
    }


{-| -}
type Reference info
    = Reference
        { index : Int
        , result : Expectation
        , info : info
        }


{-| -}
type SuiteState info
    = SuiteState (List ( info, List (Item info) ))


{-| -}
type alias Tasks info =
    List (Task Never (Reference info))


{-| -}
type alias Return info =
    ( SuiteState info, Next info )


{-| -}
type alias Validator info =
    Int -> info -> Maybe (InfoData info)


{-| -}
setup : info -> Test info -> Validator info -> Result String (Return info)
setup info suite validator =
    setup_ info suite
        |> Result.map (update validator)


{-| -}
next : Validator info -> Reference info -> SuiteState info -> Return info
next validator ref model =
    apply ref model
        |> (\m ->
                if isCritical ref then
                    ( m
                    , exit exitStatusCritical m
                        |> Maybe.map Report
                        |> Maybe.withDefault (Tasks [])
                    )

                else
                    let
                        ( newModel, tasks ) =
                            update validator m
                    in
                    ( newModel
                    , exit exitStatus newModel
                        |> Maybe.map Report
                        |> Maybe.withDefault tasks
                    )
           )


{-| -}
isFail : Reference info -> Bool
isFail (Reference { result }) =
    Result.toMaybe result == Nothing


isCritical : Reference info -> Bool
isCritical (Reference { result }) =
    case result of
        Err { critical } ->
            critical

        Ok _ ->
            False


exitStatusCritical : RunStatus -> Bool
exitStatusCritical s =
    case s of
        Done _ ->
            False

        Waiting f ->
            False

        Running ->
            True

        RunTimeSkip ->
            False


exitStatus : RunStatus -> Bool
exitStatus s =
    case s of
        Done _ ->
            False

        Waiting f ->
            True

        Running ->
            True

        RunTimeSkip ->
            False


exit : (RunStatus -> Bool) -> SuiteState info -> Maybe (Report info)
exit done (SuiteState model) =
    if Nothing == List.getFirst (\( _, a ) -> List.getFirst (\b -> done b.status) a /= Nothing) model then
        Just (makeReport (SuiteState model))

    else
        Nothing


makeReport (SuiteState model) =
    -- { desc = ["WebDriver","Step","url"], info = { capabilities = <internals>, instances = 5, url = "http://localhost:9515" }, status = Done (Ok <internals>) }
    model
        |> List.concatMap
            (\( _, items ) ->
                List.map (\item -> { desc = item.desc, info = item.info, status = runtimToStatus item.status }) items
            )


runtimToStatus s =
    -- Result { critical : Bool, error : String, context : E.Value } E.Value
    case s of
        Done (Err { critical, error, context }) ->
            Fail error

        Done (Ok context) ->
            Pass

        Waiting _ ->
            Skip

        Running ->
            Skip

        RunTimeSkip ->
            Skip


{-| -}
update : Validator info -> SuiteState info -> Return info
update validator (SuiteState model) =
    List.mapAccuml
        (\tasks ( info, queue ) ->
            let
                ( newTasks, newQueue ) =
                    take validator queue
            in
            ( tasks ++ newTasks, ( info, newQueue ) )
        )
        []
        model
        |> (\( a, b ) -> ( SuiteState b, Tasks a ))


apply : Reference info -> SuiteState info -> SuiteState info
apply (Reference data) (SuiteState model) =
    List.map
        (\( info, queue ) ->
            if info == data.info then
                ( info
                , queue |> List.updateAt data.index (\item -> { item | status = Done data.result })
                )

            else
                ( info, queue )
        )
        model
        |> SuiteState


take : Validator info -> List (Item info) -> ( Tasks info, List (Item info) )
take validator queue =
    List.indexedFoldl
        (\index item ( acc1, acc2 ) ->
            let
                running =
                    List.count (.status >> (==) Running) acc2
            in
            case ( runnable item, validator running item.info ) of
                ( Just exec, Just { url, capabilities } ) ->
                    ( acc1
                        ++ [ browser url capabilities exec
                                |> Task.map (\r -> Reference { index = index, info = item.info, result = r })
                           ]
                    , acc2 ++ [ { item | status = Running } ]
                    )

                ( Just f, Nothing ) ->
                    if List.length acc1 > 0 then
                        ( acc1, acc2 ++ [ item ] )

                    else
                        ( acc1, acc2 ++ [ { item | status = RunTimeSkip } ] )

                _ ->
                    ( acc1, acc2 ++ [ item ] )
        )
        ( [], [] )
        queue


runnable : Item info -> Maybe UnitTestFunction
runnable item =
    case item.status of
        Done _ ->
            Nothing

        Waiting f ->
            Just f

        Running ->
            Nothing

        RunTimeSkip ->
            Nothing


setup_ : info -> Test info -> Result String (SuiteState info)
setup_ info suite =
    unwrap info suite [] { only = False, skip = False } (Ok ( False, [] ))
        |> Result.map Tuple.second
        |> Result.map SuiteState


type alias Accumulator info =
    ( Bool, List ( info, List (Item info) ) )



-- ( Config, List ( info, List (Item info) ) )


type alias Config =
    { only : Bool
    , skip : Bool
    }


unwrap : info -> Test info -> List String -> Config -> Result String (Accumulator info) -> Result String (Accumulator info)
unwrap info suite path config =
    Result.andThen
        (\(( onlyMode, queues ) as acc) ->
            case suite of
                UnitTest f ->
                    let
                        newItem =
                            { info = info
                            , desc = path
                            , status = status { onlyMode = onlyMode, only = config.only, skip = config.skip } f
                            }

                        ( found, newQueue ) =
                            List.mapAccumr
                                (\found_ ( info_, items ) ->
                                    if info == info_ then
                                        ( True, ( info_, items ++ [ newItem ] ) )

                                    else
                                        ( found_, ( info_, items ) )
                                )
                                False
                                queues

                        resultQueue =
                            if found then
                                newQueue

                            else
                                queues ++ [ ( info, [ newItem ] ) ]
                    in
                    Ok ( onlyMode, resultQueue )

                Browser infos rest ->
                    List.foldl
                        (\info_ acc_ ->
                            unwrap info_ rest path config acc_
                        )
                        (Ok acc)
                        infos

                Labeled label rest ->
                    unwrap info rest (path ++ [ label ]) config (Ok acc)

                Skipped rest ->
                    unwrap info rest path { config | skip = True } (Ok acc)

                Only rest ->
                    let
                        newQueue =
                            if onlyMode then
                                queues

                            else
                                -- Update previous set statuses, as they had no clue that it is only mode,
                                -- and do that only if that is first time when only mode kicks in
                                List.map (Tuple.mapSecond (List.map (\i -> { i | status = RunTimeSkip }))) queues
                    in
                    unwrap info rest path { config | only = True } (Ok ( True, newQueue ))

                Batch suites ->
                    List.foldl (\suite_ acc_ -> unwrap info suite_ path config acc_) (Ok acc) suites

                ParseErr e ->
                    Err e
        )


status : { onlyMode : Bool, only : Bool, skip : Bool } -> UnitTestFunction -> RunStatus
status { onlyMode, only, skip } f =
    if skip || (onlyMode && not only) then
        RunTimeSkip

    else
        Waiting f
