module WebDriver.Helper.Browser
    exposing
        ( Browser
        , browsers
        , chrome
        , chromes
        , configure
        , firefox
        , firefoxes
        , headlessChrome
        , headlessChromes
        )

{-|

@docs browsers, Browser, chrome, firefox, headlessChrome, chromes, firefoxes, headlessChromes, configure

-}

import WebDriver.Helper.Capabilities as Capabilities exposing (Capabilities)
import WebDriver.Internal exposing (BrowserData, Test(..))


{-| Name and Capabilities pair that can be used to wrap tests, and report (display name) / run tests in specific browser
-}
type alias Browser =
    BrowserData


{-| Test wrapper that run tests inside in all discribed [`Browsers`](#Browser)
-}
browsers : List Browser -> Test -> Test
browsers list test =
    Browser list test


{-| Configure webdriver host in one go
-}
configure : String -> { chrome : Browser, chromes : Int -> Browser, firefox : Browser, firefoxes : Int -> Browser, headlessChrome : Browser, headlessChromes : Int -> Browser }
configure host =
    { firefox = firefox host
    , firefoxes = firefoxes host
    , chrome = chrome host
    , chromes = chromes host
    , headlessChrome = headlessChrome host
    , headlessChromes = headlessChromes host
    }


{-| -}
firefox : String -> Browser
firefox host =
    firefoxes host 1


{-| -}
firefoxes : String -> Int -> Browser
firefoxes host instances =
    { name = "FireFox"
    , capabilities =
        Capabilities.default
            |> Capabilities.withBrowser "firefox"
            |> Capabilities.withVersion "61.0"
            |> Capabilities.encode
    , instances = instances
    , dirverHost = host
    }


{-| -}
chrome : String -> Browser
chrome host =
    chromes host 1


{-| -}
chromes : String -> Int -> Browser
chromes host instances =
    { name = "Chrome"
    , capabilities =
        Capabilities.default
            |> Capabilities.withBrowser "chrome"
            |> Capabilities.encode
    , instances = instances
    , dirverHost = host
    }


{-| -}
headlessChrome : String -> Browser
headlessChrome host =
    headlessChromes host 1


{-| -}
headlessChromes : String -> Int -> Browser
headlessChromes host instances =
    let
        options =
            Capabilities.defaultChromeOptions
    in
    { name = "Chrome Headless"
    , capabilities =
        Capabilities.default
            |> Capabilities.withBrowser "chrome"
            |> Capabilities.withChromeOptions
                { options
                    | args = "--headless" :: options.args
                }
            |> Capabilities.encode
    , instances = instances
    , dirverHost = host
    }



-- android
-- internet explorer
-- iphone
-- opera
