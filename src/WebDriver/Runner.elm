module WebDriver.Runner exposing (run, runWith, configuration)

import Task exposing (Task)
import WebDriver.Internal exposing (Test(..), Command(..), Parsed(..), unwrap)
import Platform exposing (programWithFlags)
import Json.Decode as Json
import WebDriver.Loging as Log
import WebDriver.LowLevel as WebDriver exposing (Expectation(..))
import WebDriver.LowLevel.Functions as WebDriver exposing (Functions)
import Json.Encode


-- type alias Program =
--     Platform.Program Json.Value Model Message


run suite =
    runWith configuration suite


type alias Configuration =
    { dirverHost : String
    , capabilities : Json.Value
    }


configuration : Configuration
configuration =
    -- { dirverHost = "http://localhost:4444/wd/hub"
    { dirverHost = "http://localhost:9515"
    , capabilities =
        Json.Encode.object
            [ ( "desiredCapabilities"
              , Json.Encode.object
                    [ ( "browserName", Json.Encode.string "firefox" ) ]
              )
            ]
    }


runWith configs suite =
    programWithFlags
        { init = init configs suite
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { configuration : Configuration
    , commands : List Command
    , onlyMode : Bool
    }


type Message
    = TestSuccess String
    | TestFail Bool String


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        TestFail True err ->
            ( model, Log.error err )

        TestFail False err ->
            ( model, Log.error err )

        TestSuccess spaces ->
            let
                ( newCommands, newCmds ) =
                    model.commands |> doCommands model.onlyMode model.configuration
            in
                ( { model | commands = newCommands }, Cmd.batch [ newCmds, Log.successBadge spaces ] )


init configs suite flags =
    let
        configuration =
            configurationDecoder configs flags

        model =
            { configuration = configuration
            , commands = []
            , onlyMode = False
            }
    in
        case unwrap suite of
            Success onlyMode commands_ ->
                let
                    ( commands, cmds ) =
                        List.reverse commands_ |> doCommands onlyMode configuration
                in
                    ( { model
                        | commands = commands
                        , onlyMode = onlyMode
                      }
                    , cmds
                    )

            Error s ->
                ( model
                , Log.error s
                )


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model, Cmd.batch [ cmd, oldCmd ] )


doCommands : Bool -> Configuration -> List Command -> ( List Command, Cmd Message )
doCommands onlyMode config commands =
    doCommands_ onlyMode config commands ( [], Cmd.none )


doCommands_ : Bool -> Configuration -> List Command -> ( List Command, Cmd Message ) -> ( List Command, Cmd Message )
doCommands_ onlyMode config commands acc =
    case commands of
        [] ->
            acc

        (TextMe skip only value) :: [] ->
            if skip || onlyMode && not only then
                acc |> addCmd (Log.skip value)
            else
                acc |> addCmd (Log.info value)

        (TextMe skip only value) :: rest ->
            (if skip || onlyMode && not only then
                acc |> addCmd (Log.skip value)
             else
                acc |> addCmd (Log.info value)
            )
                |> doCommands_ onlyMode config rest

        (DoMe skip only spaces test) :: [] ->
            if skip || onlyMode && not only then
                acc |> addCmd (Log.skipBadge spaces)
            else
                acc |> addCmd (Log.success spaces)

        (DoMe skip only spaces test) :: rest ->
            (if skip || onlyMode && not only then
                acc |> addCmd (Log.skipBadge spaces) |> doCommands_ onlyMode config rest
             else
                let
                    cmd =
                        runOne config.dirverHost config.capabilities test
                            |> Task.perform (msgFromExpectation (TestSuccess spaces) (TestFail False) (TestFail True))
                in
                    -- acc |> addCmd (Log.success (spaces ++ "Here goes test"))
                    acc |> addCmd cmd |> Tuple.mapFirst (always rest)
            )


configurationDecoder : Configuration -> Json.Value -> Configuration
configurationDecoder configs flags =
    let
        url =
            flags
                |> Json.decodeValue (Json.field "webdriverUrl" Json.string)
                |> Result.withDefault "http://localhost:9515"

        -- sessionId =
        --     flags
        --         |> Json.decodeValue (Json.field "sessionId" Json.string)
        --         |> Result.withDefault ""
    in
        configs



-- send : msg -> Cmd msg
-- send msg =
--     Task.succeed msg
--         |> Task.perform identity


msgFromExpectation : a -> (String -> a) -> (String -> a) -> Expectation -> a
msgFromExpectation msg1 msg2 msg3 result =
    case result of
        Pass ->
            msg1

        Fail {- Exit -} False string ->
            msg2 string

        Fail {- Exit -} True string ->
            msg3 string


runOne : String -> Json.Value -> (Functions -> Task ( a1, String ) a) -> Task Never Expectation
runOne driverUrl capabilities test =
    WebDriver.browser driverUrl capabilities (\f -> test f |> Task.mapError Tuple.second)



-- runTestSequence : String -> List (Functions -> Task String a) -> Task Never Expectation
-- runTestSequence driverUrl tests =
--     (List.foldl
--         (\t acc ->
--             acc
--                 |> Task.andThen (\_ -> WebDriver.browser driverUrl t)
--         )
--         (Task.succeed Pass)
--         tests
--     )
-- runparallel : String -> (Expectation -> msg) -> b -> Cmd msg
-- runparallel driverUrl msg tests =
--     (Task.succeed Pass) |> Task.perform msg
