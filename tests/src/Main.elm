module Main exposing (main)

import Json.Decode as D
import Json.Encode as E
import Platform exposing (worker)
import Task
import Test.All
import Util
import WebDriver.Setup as WebDriver exposing (Next, Reference, Report, Status(..), SuiteState, Validator, next, setup)


type alias Config =
    { url : String
    , capabilities : E.Value
    , instances : Int
    }


config : Config
config =
    { url =
        "http://localhost:4444/wd/hub"

    -- "http://localhost:9515"
    , capabilities =
        D.decodeString D.value
            ("{ \"desiredCapabilities\": {"
                -- ++ "\"browserName\": \"firefox\""
                ++ "\"browserName\": \"chrome\""
                -- ++ ",\"version\": \"67.0\""
                ++ ",\"chromeOptions\": {\"args\": [ \"--headless\", \"--disable-gpu\", \"--window-size=800,600\" ]}"
                -- ++ ",\"moz:firefoxOptions\": {\"args\": [\"-headless\"]}"
                ++ "}}"
            )
            |> Result.withDefault E.null
    , instances = 10
    }


main : Program () Model Msg
main =
    worker
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        }


type alias Model =
    Result String (SuiteState Config)


type alias Msg =
    Reference Config


init : () -> ( Model, Cmd Msg )
init flags =
    lifeCycle (setup config Test.All.suite validator)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    Result.map (next validator msg)
        >> lifeCycle
        >> Util.printDotOrF msg


validator : Validator Config
validator a b =
    if b.instances > a then
        Just b

    else
        Nothing


lifeCycle : Result String ( SuiteState Config, Next Config ) -> ( Model, Cmd Msg )
lifeCycle income =
    case income of
        Ok ( model, WebDriver.Tasks tasks ) ->
            ( Ok model, Util.msgFromTask tasks )

        Ok ( model, WebDriver.Report report ) ->
            ( Ok model, Util.report report )

        Err e ->
            ( Err e, Util.printError e )
