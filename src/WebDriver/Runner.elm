module WebDriver.Runner exposing (Configuration, TestRunner, configuration, run, runWith)

{-|

@docs run, runWith, configuration, TestRunner, Configuration

-}

import Json.Decode as Json
import Json.Encode
import Platform exposing (programWithFlags)
import Task exposing (Task)
import WebDriver.Internal as Internal exposing (Command(..), Expectation(..), Parsed(..), unwrap)
import WebDriver.Internal.Browser as WebDriver exposing (browser)
import WebDriver.Loging as Log
import WebDriver.LowLevel.Capabilities as Capabilities exposing (Capabilities)
import WebDriver.Step as WebDriver exposing (Functions)
import WebDriver.Test exposing (Test)


{-| -}
type alias TestRunner =
    Application


type alias Application =
    Platform.Program Json.Value Model Message


{-| A function which, run tests with fallback [`configuration`](#configuration),
if flags no are defined (to define own use [`runWith`](#runWith) or define with flags)
-}
run : Test -> TestRunner
run suite =
    runWith configuration suite


{-| -}
type alias Configuration =
    { dirverHost : String
    , capabilities : Capabilities
    }


{-| Default configuration that is used in [`run`](#run)
-}
configuration : Configuration
configuration =
    { dirverHost = "http://localhost:9515"
    , capabilities = Capabilities.default
    }


{-| Same as [`run`](#run), only allows You define Your own fallbacks
-}
runWith : Configuration -> Test -> TestRunner
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
            let
                ( newCommands, newCmds ) =
                    model.commands |> doCommands model.onlyMode model.configuration
            in
            ( { model | commands = newCommands }, Cmd.batch [ newCmds, Log.error err ] )

        TestSuccess spaces ->
            let
                ( newCommands, newCmds ) =
                    model.commands |> doCommands model.onlyMode model.configuration
            in
            ( { model | commands = newCommands }, Cmd.batch [ newCmds, Log.successBadge spaces ] )


init : Configuration -> Test -> Json.Value -> ( { commands : List Command, configuration : Configuration, onlyMode : Bool }, Cmd Message )
init configs suite flags =
    let
        configuration =
            configurationInit configs flags

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
                executeDoMe config spaces test acc

        (DoMe skip only spaces test) :: rest ->
            if skip || onlyMode && not only then
                acc |> addCmd (Log.skipBadge spaces) |> doCommands_ onlyMode config rest
            else
                executeDoMe config spaces test acc
                    |> Tuple.mapFirst (always rest)


executeDoMe : Configuration -> String -> (Functions -> Task Never Expectation) -> ( model, Cmd Message ) -> ( model, Cmd Message )
executeDoMe config spaces test acc =
    let
        failparser err =
            err
                |> String.split "\n"
                |> String.join ("\n" ++ spaces)
                |> (++) spaces

        cmd =
            runOne config.dirverHost (Capabilities.encode config.capabilities) test
                |> Task.perform (msgFromExpectation (TestSuccess spaces) (failparser >> TestFail False) (TestFail True))
    in
    acc |> addCmd cmd


configurationInit : Configuration -> Json.Value -> Configuration
configurationInit configs flags =
    let
        dirverHost =
            flags
                |> Json.decodeValue (Json.field "webdriverUrl" Json.string)
                |> Result.withDefault configs.dirverHost
    in
    { configs | dirverHost = dirverHost }


msgFromExpectation : a -> (String -> a) -> (String -> a) -> Expectation -> a
msgFromExpectation msg1 msg2 msg3 result =
    case result of
        Pass ->
            msg1

        Fail {- Exit -} False string ->
            msg2 string

        Fail {- Exit -} True string ->
            msg3 string


runOne : String -> Json.Encode.Value -> (Functions -> Task Never Expectation) -> Task Never Expectation
runOne driverUrl capabilities test =
    WebDriver.browser driverUrl capabilities test
