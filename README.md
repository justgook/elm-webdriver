# Elm-WebDriver

`WebDriver` is a remote control interface that enables introspection and control of user agents. It provides a platform- and language-neutral wire protocol as a way for out-of-process programs to remotely instruct the behavior of web browsers.

`elm-webdriver` is framework that allows write scripts for `WebDriver` with plain [Node.js](https://nodejs.org/) runner that just executes compiled elm

## Suported Drivers

  - [ChromeDriver](http://chromedriver.chromium.org/)
  - [GeckoDriver](https://github.com/mozilla/geckodriver)
  - [Selenium Standalone Server](https://www.seleniumhq.org/download/)
  - [Appium](http://appium.io/)

# Setup

Create test

```elm
suite : WebDriver.Test a
suite =
    describe "Web Page Navigate"
        [ test "star elm-webdriver" <|
            \{ url, element, attribute } ->
                url "https://github.com/justgook/elm-webdriver"
                    |> Task.andThen (\_ -> "h1" |> Selector.css |> element)
                    |> Task.andThen (.value >> attribute "innerText")
                    |> Task.andThen (.value >> Expect.equal "justgook/elm-webdriver")
        ]
```

Create runner

```elm
import Json.Decode as D
import Json.Encode as E
import Platform exposing (worker)
import Task
import WebDriver.Setup as WebDriver exposing (Next, Reference, Report, Return, Status(..), SuiteState, Validator, next, setup)

port log : String -> Cmd msg


type alias Config =
    { url : String
    , capabilities : E.Value
    , instances : Int
    }

config : Config
config =
    { url =
        "http://localhost:4444/wd/hub"
    , capabilities =
        D.decodeString D.value
            ("{ \"desiredCapabilities\": {"
                ++ "\"browserName\": \"chrome\""
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
        >> printDotOrF msg


validator : Validator Config
validator a b =
    if b.instances > a then
        Just b

    else
        Nothing

printDotOrF : Reference info -> ( model, Cmd msg ) -> ( model, Cmd msg )
printDotOrF ref =
    (if isFail ref then
        ansi.red ++ "F" ++ ansi.reset

     else
        ansi.green ++ "." ++ ansi.reset
    )
        |> log
                |> addCmd


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmd ( model, oldCmd ) =
    ( model, Cmd.batch [ cmd, oldCmd ] )


lifeCycle : Result String ( SuiteState Config, Next Config ) -> ( Model, Cmd Msg )
lifeCycle income =
    case income of
        Ok ( model, WebDriver.Tasks tasks ) ->
            ( Ok model, msgFromTask tasks )

        Ok ( model, WebDriver.Report report ) ->
            ( Ok model, Cmd.none )

        Err e ->
            ( Err e, log e )

msgFromTask : List (Task Never msg) -> Cmd msg
msgFromTask =
    List.map (Task.perform identity) >> Cmd.batch
```
Run From CLI

  >Note: To be able run from node you need install `XMLHttpRequest` replacement (node don't have build in). Install `xhr2` and append it:

  `var XMLHttpRequest = require(\"xhr2\");`

compile Your tests

`elm-make Main.elm --output=bundle.js`

and run compiled code

`node -e 'var XMLHttpRequest = require(\"xhr2\"); require(\"./bundle.js\").Main.worker().ports.log.subscribe((a)=>process.stdout.write(a))'`
