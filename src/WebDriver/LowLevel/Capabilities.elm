module WebDriver.LowLevel.Capabilities exposing (Capabilities, ChromeOptionsData, default, defaultChromeOptions, encode, withBrowser, withChromeOptions)

{-|

@docs Capabilities, default, withBrowser, encode, withChromeOptions, defaultChromeOptions, ChromeOptionsData

-}

import Json.Encode as Encode


{-| -}
type Capabilities
    = Capabilities
        { browserName : String
        , javascriptEnabled : Bool
        , platform : String
        }
    | ChromeCapabilities
        { browserName : String
        , javascriptEnabled : Bool
        , platform : String
        , chromeOptions : ChromeOptionsData
        }


{-| Special options for chrome, that can extend functionality of chrome browser
-}
type alias ChromeOptionsData =
    { androidPackage : String --"com.google.android.apps.chrome",
    , args : List String
    , extensions : List String
    }


{-| Encodes Capabilities for sending to WebDriver
-}
encode : Capabilities -> Encode.Value
encode caps =
    case caps of
        Capabilities data ->
            baseEncode (always []) data

        ChromeCapabilities data ->
            baseEncode (always []) data


baseEncode : ({ a | browserName : String, javascriptEnabled : Bool } -> List ( String, Encode.Value )) -> { a | browserName : String, javascriptEnabled : Bool } -> Encode.Value
baseEncode addition data =
    let
        --   [ ( "browserName", Encode.string "Safari" )
        -- , ( "platformName", Encode.string "iOS" )
        -- , ( "deviceName", Encode.string "iPhone Simulator" )
        -- , ( "automationName", Encode.string "XCUITest" )
        -- , ( "javascriptEnabled", Encode.bool data.javascriptEnabled )
        result =
            [ ( "browserName", Encode.string data.browserName )
            , ( "javascriptEnabled", Encode.bool data.javascriptEnabled )
            ]
                ++ addition data
    in
    Encode.object [ ( "desiredCapabilities", Encode.object result ) ]


{-| Default capabilities
-}
default : Capabilities
default =
    Capabilities
        { browserName = "chrome"
        , platform = "ANY"
        , javascriptEnabled = True
        }


{-| -}
defaultChromeOptions : ChromeOptionsData
defaultChromeOptions =
    { androidPackage = ""
    , args = []
    , extensions = []
    }


{-| Set chromeOptions
-}
withChromeOptions : ChromeOptionsData -> Capabilities -> Capabilities
withChromeOptions options base_ =
    case base_ of
        Capabilities base ->
            ChromeCapabilities
                { browserName = base.browserName
                , javascriptEnabled = base.javascriptEnabled
                , platform = base.platform
                , chromeOptions = options
                }

        ChromeCapabilities base ->
            ChromeCapabilities { base | chromeOptions = options }


{-| -}
withBrowser : String -> Capabilities -> Capabilities
withBrowser browserName caps =
    case caps of
        Capabilities data ->
            Capabilities { data | browserName = browserName }

        ChromeCapabilities data ->
            ChromeCapabilities { data | browserName = browserName }



--   "desiredCapabilities": {
--       "browserName": "chrome",
--       "chromeOptions": {
--          "androidPackage": "com.google.android.apps.chrome",
--          "args": [  ],
--          "extensions": [  ],
--          "prefs": {
--             "profile.content_settings.exceptions.fullscreen": {
--                "172.31.18.18": {
--                   "setting": 1
--                }
--             }
--          }
--       },
--       "javascriptEnabled": true,
--       "platform": "ANY",
--       "version": ""
--    }
-- }
-- {
--   "desiredCapabilities":
-- 	{
--   		"browserName": "chrome",
--   		"version": "",
--   		"platform": "ANY",
--   		"javascriptEnabled": false,
--   		"takesScreenshot": false,
--   		"handlesAlerts": false,
--   		"databaseEnabled": false,
--   		"locationContextEnabled": false,
--   		"applicationCacheEnabled": false,
--   		"browserConnectionEnabled": true,
--   		"cssSelectorsEnabled": false,
--   		"webStorageEnabled": false,
--   		"rotatable": false,
--   		"acceptSslCerts": false,
--   		"nativeEvents": true,
--         "acceptInsecureCerts": false,
--         "proxy": {"proxyType":"direct"},
--         "chromeOptions":{ "args": ["--headless"]}
-- 	}
-- }
-- chromeOptions : ChromeOptions -> Capabilities -> Capabilities
-- chromeOptions (ChromeOptions options) (Capabilities caps) =
--     Capabilities
--         { caps
--             | chromeOptions =
--                 { -- androidPackage = "com.google.android.apps.chrome"
--                   -- ,
--                   args = options.args
--                 , extensions = options.extensions
--                 --  "prefs": {
--                 --     "profile.content_settings.exceptions.fullscreen": {
--                 --        "172.31.18.18": {
--                 --           "setting": 1
--                 --        }
--                 --     }
--                 --  }
--                 }
--         }
