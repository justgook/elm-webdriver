module WebDriver.Helper.Capabilities
    exposing
        ( Capabilities
        , ChromeOptionsData
        , default
        , defaultChromeOptions
        , encode
        , withBrowser
        , withChromeOptions
        , withVersion
        )

{-|

@docs Capabilities, default, withBrowser, withVersion, encode, withChromeOptions, defaultChromeOptions, ChromeOptionsData

-}

import Json.Encode as Encode


{-| -}
type Capabilities
    = Capabilities
        { browserName : String
        , version : String
        , platform : String
        }
    | ChromeCapabilities
        { browserName : String
        , version : String
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


baseEncode : ({ a | browserName : String, version : String } -> List ( String, Encode.Value )) -> { a | browserName : String, version : String } -> Encode.Value
baseEncode addition data =
    let
        --   [ ( "browserName", Encode.string "Safari" )
        -- , ( "platformName", Encode.string "iOS" )
        -- , ( "deviceName", Encode.string "iPhone Simulator" )
        -- , ( "automationName", Encode.string "XCUITest" )
        -- , ( "javascriptEnabled", Encode.bool data.javascriptEnabled )
        result =
            [ ( "browserName", Encode.string data.browserName )
            , ( "version", Encode.string data.version )
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
        , version = "68.0"
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
                , version = base.version
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


{-| -}
withVersion : String -> Capabilities -> Capabilities
withVersion version caps =
    case caps of
        Capabilities data ->
            Capabilities { data | version = version }

        ChromeCapabilities data ->
            ChromeCapabilities { data | version = version }



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
