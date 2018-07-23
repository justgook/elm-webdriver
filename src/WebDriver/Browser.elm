module WebDriver.Browser
    exposing
        ( chrome
        )

import WebDriver.Internal exposing (Test(Browser))
import WebDriver.LowLevel.Capabilities as Capabilities exposing (Capabilities)


browsers : List ( String, Capabilities ) -> Test -> Test
browsers list test =
    Browser list test


chrome =
    ( "Chrome"
    , Capabilities.default
        |> Capabilities.withBrowser "chrome"
    )


headlessChrome =
    chrome



-- android
-- chrome
-- firefox
-- internet explorer
-- iphone
-- opera
