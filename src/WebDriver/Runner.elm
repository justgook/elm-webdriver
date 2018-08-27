module WebDriver.Runner exposing (Browsers, Expectation, Status(..), allDone, prepare, prepareNext, runOne, updateDone)

import Array exposing (Array)
import Json.Encode as Json
import NestedSet exposing (NestedSet)
import Task exposing (Task)
import WebDriver exposing (Test)
import WebDriver.Internal exposing (Test(..))
import WebDriver.Internal.Browser as WebDriver
import WebDriver.Step exposing (Functions)


type alias Executable =
    Functions -> Task Never (Result String ())


type alias Browsers info =
    { onlyMode : Bool
    , tests :
        Array (List { info : info, descId : Int, status : Status, exec : Executable })
    }


type alias Expectation =
    --Result { critical : Bool, error : String, capabilities : Json.Value } Json.Value
    WebDriver.Internal.Expectation


type Status
    = Pass Json.Value
    | Fail String Json.Value
    | Skip
    | Waiting
    | Running


allDone (( desc, tests ) as model) =
    tests.tests
        |> Array.foldr
            (\list acc ->
                getFirst (\i -> i.status == Waiting || i.status == Running) list
                    |> Maybe.map (always True)
                    |> Maybe.withDefault acc
            )
            False
        |> not


updateDone result =
    statusFromResult result |> updateStatus


getFirst validation =
    let
        getFirst_ index list =
            case list of
                [] ->
                    Nothing

                x :: [] ->
                    if validation x then
                        Just { testId = index, task = x }

                    else
                        Nothing

                x :: xs ->
                    if validation x then
                        Just { testId = index, task = x }

                    else
                        getFirst_ (index + 1) xs
    in
    getFirst_ 0


prepareNext queueId (( desc, tests ) as model) =
    tests.tests
        |> Array.get queueId
        |> Maybe.andThen (getFirst (.status >> (==) Waiting))
        |> Maybe.map
            (\({ testId, task } as cmd) ->
                ( updateStatus Running queueId testId model
                , { info = task.info
                  , testId = testId
                  , exec = task.exec
                  }
                )
            )


updateStatus status queueId testId ( desc, { onlyMode, tests } as data ) =
    ( desc
    , { data
        | tests =
            tests
                |> updateArray queueId
                    (updateList testId (\item -> { item | status = status }))
      }
    )


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs



-- https://github.com/elm-community/list-extra/blob/8.0.0/src/List/Extra.elm


updateList : Int -> (a -> a) -> List a -> List a
updateList index fn list =
    if index < 0 then
        list

    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                head ++ fn x :: xs

            _ ->
                list


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray n f a =
    let
        element =
            Array.get n a
    in
    case element of
        Nothing ->
            a

        Just element_ ->
            Array.set n (f element_) a


statusFromResult r =
    case r of
        Err { error, capabilities } ->
            Fail error capabilities

        Ok capabilities ->
            Pass capabilities


prepare : info -> Test info -> Result String ( NestedSet String, Browsers info )
prepare config suite =
    unwrap config suite
        |> Result.map
            (\{ onlyMode, data, queues } ->
                ( data
                , { onlyMode = onlyMode
                  , tests = queues
                  }
                )
            )


runOne : String -> Json.Value -> (Functions -> Task Never (Result String ())) -> Task Never Expectation
runOne url capabilities test =
    WebDriver.browser url capabilities test


unwrap info current =
    let
        configs =
            { info = info
            , skip = False
            , only = False
            }

        model =
            ( [], Ok { onlyMode = False, data = NestedSet.empty, queues = Array.empty } )
    in
    case unwrapFold current -1 configs model of
        ( queue, Ok result ) ->
            let
                newBrowsers =
                    -- List.map (\caps -> ( caps, queue )) info
                    [ queue ]
                        |> Array.fromList
                        |> Array.append result.queues
            in
            Ok { result | queues = newBrowsers }

        ( _, Err err ) ->
            Err err


unwrapFold branch parentId ({ skip, only, info } as configs) ( queue, acc ) =
    case ( branch, acc ) of
        ( _, Err a ) ->
            ( queue, acc )

        ( UnitTest test, Ok ({ onlyMode, data } as model) ) ->
            let
                status =
                    if onlyMode && not only then
                        Skip

                    else
                        Waiting
            in
            ( queue ++ [ { descId = parentId, status = status, exec = test, info = info } ], Ok model )

        ( Browser caps rest, Ok { onlyMode, data } ) ->
            -- List.foldr (\a subAcc-> ) () caps
            -- case unwrapFold rest parentId { configs | info = caps } ( [], acc ) of
            --     ( subQueue, Ok result ) ->
            --         let
            --             newBrowsers =
            --                 -- List.map (\caps_ -> ( caps_, subQueue )) caps
            --                 [ subQueue ]
            --                     |> Array.fromList
            --                     |> Array.append result.queues
            --         in
            --         ( queue, Ok { result | queues = newBrowsers } )
            --     ( _, Err err ) ->
            ( [], Err "Not Implemented yet" )

        ( Labeled text rest, Ok ({ onlyMode, data } as model) ) ->
            let
                itemId =
                    NestedSet.length data

                newAcc =
                    Ok { model | data = NestedSet.insert parentId text data }
            in
            unwrapFold rest itemId configs ( queue, newAcc )

        ( Skipped rest, Ok { onlyMode, data } ) ->
            unwrapFold rest parentId { configs | skip = True } ( queue, acc )

        ( Only rest, Ok model ) ->
            let
                newQueue =
                    if model.onlyMode then
                        queue

                    else
                        -- Update previous set statuses, as they had no clue that it is only mode,
                        -- and do that only if that is first time when only mode kicks in
                        queue
                            |> List.map (\i -> { i | status = Skip })
            in
            unwrapFold rest parentId { configs | only = True } ( newQueue, Ok { model | onlyMode = True } )

        ( Batch listRest, Ok { onlyMode, data } ) ->
            listRest
                |> List.foldl (\rest acc2 -> unwrapFold rest parentId configs acc2) ( queue, acc )

        ( ParseErr err, Ok { onlyMode, data } ) ->
            ( queue, Err err )
