# Now

## Getting Started

To run the example tests in this repo, do the following.

1. Download
   [ChromeDriver](http://chromedriver.chromium.org/getting-started)
1. Follow the instructions in the url above to put the ChromeDriver
   binary (the file you downloaded) into your PATH (google something
   like 'how to put a file in your path' to learn more)
1. Install yarn from [here](https://yarnpkg.com/lang/en/docs/install/)
1. Clone this repo (for more info on how to clone a repo, checkout
   [Atlassian's
   tutorial](https://confluence.atlassian.com/bitbucket/clone-a-repository-223217891.html))
1. `cd` into `elm-webdriver/test`
1. Run `yarn && yarn test`
    1. If you get an error similar to `Problem with Webdriver host (Unable
        to make a connection. Is your network working?)`, you may need to
        run ChromeDriver manually. Go
        [here](http://chromedriver.chromium.org/getting-started) and find
        where it describes how to 'Start the ChromeDriver server
        separately'.

# For Publishing

# Elm-WebDriver

`WebDriver` is a remote control interface that enables introspection and control of user agents. It provides a platform- and language-neutral wire protocol as a way for out-of-process programs to remotely instruct the behavior of web browsers.

`elm-webdriver` is framework that allows write scripts for `WebDriver` with plain [Node.js](https://nodejs.org/) runner that just executes compiled elm

## Suported Drivers

  - [ChromeDriver](http://chromedriver.chromium.org/)
  - [GeckoDriver](https://github.com/mozilla/geckodriver)
  - [Selenium Standalone Server](https://www.seleniumhq.org/download/)
  - [Appium](http://appium.io/)

# Setup

## Quick Start
```elm
import Task
import WebDriver
import WebDriver.Runner exposing (TestRunner, run)

suite : WebDriver.Test
suite =
    describe "Web Page Navigate"
        [ test "star elm-webdriver" <|
            \{ url, element, attribute } ->
                url "https://github.com/justgook/elm-webdriver"
                    |> Task.andThen (\_ -> "h1" |> Selector.css |> element)
                    |> Task.andThen (.value >> attribute "innerText")
                    |> Task.andThen (.value >> Expect.equal "justgook/elm-webdriver")
        ]

main : TestRunner
main = run suite
```

## Running From CLI

  >Note: To be able run from node you need install `XMLHttpRequest` replacement (node don't have build in). Install `xhr2` and append it:

  `var XMLHttpRequest = require(\"xhr2\");`

compile Your tests

`elm-make Main.elm --output=bundle.js`

and run compiled code

`node -e 'var XMLHttpRequest = require(\"xhr2\"); require(\"./bundle.js\").Main.worker().ports.log.subscribe((a)=>process.stdout.write(a))'`
