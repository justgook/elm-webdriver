module WebDriver.Browser
    exposing
        ( Browser
        , browsers
        , chrome
        , chromes
        , firefox
        , firefoxes
        , headlessChrome
        , headlessChromes
        )

{-|

@docs browsers, Browser, chrome, firefox, headlessChrome, chromes, firefoxes, headlessChromes

-}

import WebDriver.Internal exposing (BrowserData, Test(Browser))
import WebDriver.LowLevel.Capabilities as Capabilities exposing (Capabilities)


{-| Name and Capabilities pair that can be used to wrap tests, and report (display name) / run tests in specific browser
-}
type alias Browser =
    BrowserData


{-| Test wrapper that run tests inside in all discribed [`Browsers`](#Browser)
-}
browsers : List Browser -> Test -> Test
browsers list test =
    Browser list test


{-| -}
firefox : Browser
firefox =
    firefoxes 1


{-| -}
firefoxes : Int -> Browser
firefoxes instances =
    { name = "FireFox"
    , capabilities =
        Capabilities.default
            |> Capabilities.withBrowser "firefox"
            |> Capabilities.encode
    , instances = instances
    , dirverHost = "http://localhost:4444/wd/hub"
    }


{-| -}
chrome : Browser
chrome =
    chromes 1


{-| -}
chromes : Int -> Browser
chromes instances =
    { name = "Chrome"
    , capabilities =
        Capabilities.default
            |> Capabilities.withBrowser "chrome"
            |> Capabilities.encode
    , instances = instances
    , dirverHost = "http://localhost:4444/wd/hub"
    }


{-| -}
headlessChrome : Browser
headlessChrome =
    headlessChromes 1


{-| -}
headlessChromes : Int -> Browser
headlessChromes instances =
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
    , dirverHost = "http://localhost:4444/wd/hub"
    }



-- android
-- internet explorer
-- iphone
-- opera
