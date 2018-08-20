module WebDriver.Step
    exposing
        ( Functions
        , Host
        , WithSession
        , actions
        , addCookie
        , alertAccept
        , alertDismiss
        , alertText
        , attribute
        , back
        , clear
        , click
        , close
        , cookie
        , cookies
        , css
        , deleteCookie
        , deleteCookies
        , element
        , elementInElement
        , elementScreenshot
        , elements
        , elementsInElement
        , enabled
        , execute
        , executeAsync
        , forward
        , frame
        , frameParent
        , fullscreen
        , functions
        , getTimeouts
        , getUrl
        , getWindowRect
        , maximize
        , minimize
        , promptText
        , property
        , rect
        , refresh
        , release
        , screenshot
        , selected
        , sessionStart
        , sessionStop
        , setTimeouts
        , setWindowRect
        , status
        , tagName
        , text
        , title
        , url
        , value
        , window
        , windowHandle
        , windowHandles
        )

{-| ##Location Navigation
@docs url, getUrl

##Elements
@docs element, elements, click, clear, value, elementInElement, elementsInElement, selected, enabled, tagName, text, attribute, property , css, rect, elementScreenshot

##History Navigation
@docs back, forward, refresh

##Window
@docs title, windowHandle, windowHandles, window, getWindowRect, setWindowRect, fullscreen, maximize, minimize, close, screenshot

#Alerts
@docs alertAccept, alertDismiss, alertText, promptText

##Inject a snippet of JavaScript
@docs execute, executeAsync

##Frames / iFrames
@docs frameParent, frame

##Timeouts
@docs getTimeouts, setTimeouts

##Cookies
@docs cookies, cookie, deleteCookies, deleteCookie, addCookie

##Actions

@docs actions, release

##Webdriver Host info
@docs status

#Low level function
@docs Functions,Host, WithSession, functions, sessionStart, sessionStop

-}

import Http
import Json.Encode as Json
import Task exposing (Task)
import WebDriver.Internal.HttpHelper as Http exposing (toTask)
import WebDriver.Internal.Value as Value exposing (Answer, Cookie, Out, WindowHandle(Handle), answerDecoder, jsonFromSelector)
import WebDriver.Internal.Value.Action as Action
import WebDriver.Internal.Value.Status as Status exposing (Status)
import WebDriver.Internal.Value.Timeouts as Timeouts exposing (Timeouts)
import WebDriver.Step.Action exposing (Action)
import WebDriver.Step.Element exposing (Element, Selector)


type alias Step value =
    Task String (Answer value)


{-| Configuration containing url to WebDriver host, used in

  - [sessionStart](#sessionStart)
  - [status](#status)

-}
type alias Host =
    { url : String }


{-| Url to WebDriver host and current browsing session
used in most of step functins
-}
type alias WithSession =
    { sessionId : String, url : String }


{-| Functions that is passed to [tests](../WebDriver#test), with already created session
-}
type alias Functions =
    { back : Step ()
    , close : Step ()
    , forward : Step ()
    , getTimeouts : Step Timeouts
    , getUrl : Step String
    , refresh : Step ()
    , setTimeouts : Timeouts -> Step Timeouts
    , status : Step Status
    , title : Step String
    , url : String -> Step ()
    , windowHandle : Step WindowHandle
    , windowHandles : Step (List WindowHandle)
    , window : WindowHandle -> Step ()
    , fullscreen : Step ()
    , maximize : Step ()
    , minimize : Step ()
    , frameParent : Step ()
    , frame : Json.Value -> Step ()
    , element : Selector -> Step Element
    , elements : Selector -> Step (List Element)
    , attribute : String -> Element -> Step String
    , css : String -> Element -> Step String
    , enabled : Element -> Step Bool
    , property : String -> Element -> Step String
    , rect : Element -> Step { height : Int, width : Int, x : Int, y : Int }
    , selected : Element -> Step Bool
    , tagName : Element -> Step String
    , text : Element -> Step String
    , elementInElement : Selector -> Element -> Step Element
    , elementsInElement : Selector -> Element -> Step (List Element)
    , getWindowRect : Step { height : Int, width : Int, x : Int, y : Int }
    , setWindowRect : { height : Int, width : Int, x : Int, y : Int } -> Step { height : Int, width : Int, x : Int, y : Int }
    , clear : Element -> Step ()
    , click : Element -> Step ()
    , value : String -> Element -> Step ()
    , execute : String -> List Json.Value -> Step Json.Value
    , executeAsync : String -> List Json.Value -> Step Json.Value
    , addCookie : String -> String -> Step ()
    , cookie : String -> Step Cookie
    , cookies : Step (List Cookie)
    , deleteCookie : String -> Step ()
    , deleteCookies : Step ()
    , alertAccept : Step ()
    , alertDismiss : Step ()
    , alertText : Step String
    , promptText : String -> Step ()
    , elementScreenshot : Element -> Step String
    , screenshot : Step String
    , actions : List Action -> Step ()
    , release : Step ()
    }


{-| -}
functions : WithSession -> Functions
functions options =
    let
        args =
            { url = options.url }
    in
    { url = \data -> url options data |> toTask
    , status = status args |> toTask
    , getUrl = getUrl options |> toTask
    , getTimeouts = getTimeouts options |> toTask
    , setTimeouts = \data -> setTimeouts options data |> toTask
    , back = back options |> toTask
    , forward = forward options |> toTask
    , refresh = refresh options |> toTask
    , title = title options |> toTask
    , windowHandle = windowHandle options |> toTask
    , windowHandles = windowHandles options |> toTask
    , close = close options |> toTask
    , window = \data -> window options data |> toTask
    , fullscreen = fullscreen options |> toTask
    , maximize = maximize options |> toTask
    , minimize = minimize options |> toTask
    , frameParent = frameParent options |> toTask
    , frame = \data -> frame options data |> toTask
    , element = \data -> element options data |> toTask
    , elements = \data -> elements options data |> toTask
    , selected = \data -> selected options data |> toTask
    , enabled = \data -> enabled options data |> toTask
    , tagName = \data -> tagName options data |> toTask
    , text = \data -> text options data |> toTask
    , attribute = \string elm -> attribute options string elm |> toTask
    , property = \string elm -> property options string elm |> toTask
    , css = \string elm -> css options string elm |> toTask
    , elementInElement = \selector elm -> elementInElement options selector elm |> toTask
    , elementsInElement = \selector elm -> elementsInElement options selector elm |> toTask
    , getWindowRect = getWindowRect options |> toTask
    , setWindowRect = \data -> setWindowRect options data |> toTask
    , rect = \elm -> rect options elm |> toTask
    , click = \elm -> click options elm |> toTask
    , value = \string elm -> value options string elm |> toTask
    , clear = \elm -> clear options elm |> toTask
    , execute = \script args -> execute options script args |> toTask
    , executeAsync = \script args -> execute options script args |> toTask
    , cookies = cookies options |> toTask
    , cookie = \name -> cookie options name |> toTask
    , deleteCookies = deleteCookies options |> toTask
    , deleteCookie = \data -> deleteCookie options data |> toTask
    , addCookie = \name value -> addCookie options name value |> toTask
    , promptText = \value -> promptText options value |> toTask
    , alertText = alertText options |> toTask
    , alertDismiss = alertDismiss options |> toTask
    , alertAccept = alertAccept options |> toTask
    , screenshot = screenshot options |> toTask
    , elementScreenshot = \elm -> elementScreenshot options elm |> toTask
    , actions = \value -> actions options value |> toTask
    , release = release options |> toTask
    }


{-| Creates a new session with the desired capabilities.
and task result in info of created session as Json.Value

> Note: Most of cases you don't need it runner will crates session for you and stop, after test is done

-}
sessionStart : Host -> Json.Value -> Out Json.Value
sessionStart settings capabilities =
    capabilities
        |> Http.jsonBody
        |> (\body ->
                Http.post (settings.url ++ "/session") body answerDecoder.value
           )


{-| Stops session created with [`sessionStart`](#sessionStart) functon

> Note: Most of cases you don't need it runner will crates session for you and stop, after test is done

-}
sessionStop : WithSession -> Out ()
sessionStop settings =
    Http.delete (settings.url ++ "/session/" ++ settings.sessionId)
        answerDecoder.empty


{-| Query the server’s current status. The server should respond with a general “HTTP 200 OK” response if it is alive and accepting commands. The response body should be a JSON object describing the state of the server. All server implementations should return two basic objects describing the server’s current platform and when the server was built. All fields are optional; if omitted, the client should assume the value is unknown. Furthermore, server implementations may include additional fields not listed here.
-}
status : Host -> Out Status
status settings =
    Http.get (settings.url ++ "/status") answerDecoder.status


{-| -}
getTimeouts : WithSession -> Out Timeouts
getTimeouts settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/timeouts")
        answerDecoder.getTimeouts


{-|

  - `script` - session script timeout - Determines when to interrupt a script that is being evaluated.
  - `pageLoad` - session page load timeout - Provides the timeout limit used to interrupt navigation of the browsing context.
  - `implicit`- session implicit wait timeout - Gives the timeout of when to abort locating an element.

Configure the amount of time that a particular type of operation can execute for before they are aborted and a Timeout error is returned to the client.

-}
setTimeouts : WithSession -> Timeouts -> Out Timeouts
setTimeouts settings value =
    Json.object
        [ ( "script", Json.int value.script )
        , ( "pageLoad", Json.int value.pageLoad )
        , ( "implicit", Json.int value.implicit )
        ]
        |> Http.jsonBody
        |> (\body ->
                Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/timeouts")
                    body
                    answerDecoder.setTimeouts
           )


{-| Load the URL of the browser.
-}
url : WithSession -> String -> Out ()
url settings url_ =
    Json.object
        [ ( "url"
          , Json.string url_
          )
        ]
        |> Http.jsonBody
        |> (\body ->
                Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/url")
                    body
                    answerDecoder.empty
           )


{-| Get the url of current opened website.
-}
getUrl : WithSession -> Out String
getUrl settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/url")
        answerDecoder.string


history_ : String -> WithSession -> Http.Request (Answer ())
history_ path settings =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/" ++ path)
        Http.emptyBody
        answerDecoder.empty


{-| Navigate backwards in the browser history, if possible.
-}
back : WithSession -> Out ()
back =
    history_ "back"


{-| Navigate forwards in the browser history, if possible.
-}
forward : WithSession -> Out ()
forward =
    history_ "forward"


{-| Refresh the current page.
-}
refresh : WithSession -> Out ()
refresh =
    history_ "refresh"


{-| Get the title of current opened website.
-}
title : WithSession -> Out String
title settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/title")
        answerDecoder.string


{-| Get handle with tabs in the browser.
-}
windowHandle : WithSession -> Out WindowHandle
windowHandle settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/window")
        answerDecoder.windowHandle


{-| Retrieve the list of all window handles available to the session.
-}
windowHandles : WithSession -> Out (List WindowHandle)
windowHandles settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/window/handles")
        answerDecoder.windowHandles


{-| Close current window (and focus on an other window).
-}
close : WithSession -> Out ()
close settings =
    Http.delete (settings.url ++ "/session/" ++ settings.sessionId ++ "/window")
        answerDecoder.empty


{-| Switching window will select the current top-level browsing context used as the target for all subsequent commands.
In a tabbed browser, this will typically make the tab containing the browsing context the selected tab.
-}
window : WithSession -> WindowHandle -> Out ()
window settings (Handle handle) =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/window")
        (Json.object
            [ ( "name"
              , Json.string handle
              )
            ]
            |> Http.jsonBody
        )
        answerDecoder.empty


window_ : String -> WithSession -> Out ()
window_ path settings =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/" ++ path)
        Http.emptyBody
        answerDecoder.empty


{-| The Fullscreen Window command invokes the window manager-specific “full screen” operation,
if any, on the window containing the current top-level browsing context.
This typically increases the window to the size of the physical display
and can hide browser UI elements such as toolbars.

> _Note_: this command was recently added to the official Webdriver protocol
> and might not be working with current Selenium driver.

-}
fullscreen : WithSession -> Out ()
fullscreen =
    window_ "fullscreen"


{-| The Maximize Window command invokes the window manager-specific “maximize” operation,
if any, on the window containing the current top-level browsing context.
This typically increases the window to the maximum available size without going full-screen.
-}
maximize : WithSession -> Out ()
maximize =
    window_ "maximize"


{-| The Minimize Window command invokes the window manager-specific “minimize” operation,
if any, on the window containing the current top-level browsing context.
This typically hides the window in the system tray.
-}
minimize : WithSession -> Out ()
minimize =
    window_ "minimize"


{-| The Get Window Rect command returns the size and position
on the screen of the operating system window corresponding
to the current top-level browsing context.
-}
getWindowRect : WithSession -> Out { height : Int, width : Int, x : Int, y : Int }
getWindowRect settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/window/rect")
        answerDecoder.decodeRect


{-| The Set Window Rect command alters the size and the position
of the operating system window corresponding
to the current top-level browsing context.
-}
setWindowRect : WithSession -> { height : Int, width : Int, x : Int, y : Int } -> Out { height : Int, width : Int, x : Int, y : Int }
setWindowRect settings { height, width, x, y } =
    Http.post
        (settings.url ++ "/session/" ++ settings.sessionId ++ "/window/rect")
        (Json.object
            [ ( "height", Json.int height )
            , ( "width", Json.int width )
            , ( "x", Json.int x )
            , ( "y", Json.int y )
            ]
            |> Http.jsonBody
        )
        answerDecoder.decodeRect


{-| Take a screenshot of the current viewport.
-}
screenshot : WithSession -> Out String
screenshot settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/screenshot")
        answerDecoder.string


{-| Search for an element on the page, starting from the document root.
The located element will be returned as a `Element`.
-}
element : WithSession -> Selector -> Out Element
element settings selector =
    Http.post
        (settings.url ++ "/session/" ++ settings.sessionId ++ "/element")
        (jsonFromSelector selector |> Http.jsonBody)
        answerDecoder.element


{-| Search for multiple elements on the page, starting from the document root.
The located elements will be returned as a `List Element`.
Elements should be returned in the order located in the DOM.
-}
elements : WithSession -> Selector -> Out (List Element)
elements settings selector =
    Http.post
        (settings.url ++ "/session/" ++ settings.sessionId ++ "/elements")
        (jsonFromSelector selector |> Http.jsonBody)
        answerDecoder.elements


{-| Search for an element on the page, starting from an element.
The located element will be returned as a `Element`.
-}
elementInElement : WithSession -> Selector -> Element -> Out Element
elementInElement settings selector (Value.Element element) =
    -- POST	/session/{session id}/element/{element id}/element	Find Element From Element
    Http.post
        (settings.url ++ "/session/" ++ settings.sessionId ++ "/element/" ++ element ++ "/element")
        (jsonFromSelector selector |> Http.jsonBody)
        answerDecoder.element


{-| Search for multiple elements on the page, starting from an element.
The located elements will be returned as a `List Element`.
Elements should be returned in the order located in the DOM.
-}
elementsInElement : WithSession -> Selector -> Element -> Out (List Element)
elementsInElement settings selector (Value.Element element) =
    -- POST	/session/{session id}/element/{element id}/elements	Find Elements From Element
    Http.post
        (settings.url ++ "/session/" ++ settings.sessionId ++ "/element/" ++ element ++ "/elements")
        (jsonFromSelector selector |> Http.jsonBody)
        answerDecoder.elements


requestElemntProp_ : String -> Value.AnswerDecoder a -> WithSession -> Element -> Out a
requestElemntProp_ path decoder settings (Value.Element element) =
    Http.get
        (settings.url ++ "/session/" ++ settings.sessionId ++ "/element/" ++ element ++ "/" ++ path)
        decoder


{-| The Is Element Selected command determines
if the referenced element is selected or not.
This operation only makes sense on input elements
of the Checkbox- and Radio Button states, or on option elements.
-}
selected : WithSession -> Element -> Out Bool
selected =
    requestElemntProp_ "selected" answerDecoder.bool


{-| Determine if an element is currently enabled.
-}
enabled : WithSession -> Element -> Out Bool
enabled =
    requestElemntProp_ "enabled" answerDecoder.bool


{-| Get tag name of a DOM-element.
-}
tagName : WithSession -> Element -> Out String
tagName =
    requestElemntProp_ "name" answerDecoder.string


{-| Get the text content from a DOM-elemen.
Make sure the element you want to request the text from is interactable
otherwise you will get an empty string as return value.
If the element is disabled or not visible and you still want to receive the text content use `getHTML` (TODO) as a workaround.
-}
text : WithSession -> Element -> Out String
text =
    requestElemntProp_ "text" answerDecoder.string


{-| Get an attribute from a DOM-element based attribute name.
Returns a attribute value.
-}
attribute : WithSession -> String -> Element -> Out String
attribute settings attr element =
    requestElemntProp_ ("attribute/" ++ attr) answerDecoder.string settings element


{-| Get the value of an element’s property.
-}
property : WithSession -> String -> Element -> Out String
property settings prop element =
    requestElemntProp_ ("property/" ++ prop) answerDecoder.string settings element


{-| Get a css property from a DOM-element.
The return value is not formatted.

> Note that shorthand CSS properties (e.g. background, font, border, margin, padding, list-style, outline, pause, cue) are not returned, in accordance with the DOM CSS2 specification - you should directly access the longhand properties (e.g. background-color) to access the desired values.

-}
css : WithSession -> String -> Element -> Out String
css settings prop element =
    requestElemntProp_ ("css/" ++ prop) answerDecoder.string settings element


{-| The Get Element Rect command returns the dimensions and coordinates of the given web element.
The returned value is a record with `x`, `y`, `width` and `height` properties.

> Note: this command was recently added to the official Webdriver protocol
> and might not be working with current Selenium driver.

-}
rect : WithSession -> Element -> Out { height : Int, width : Int, x : Int, y : Int }
rect settings element =
    requestElemntProp_ "rect" answerDecoder.decodeRect settings element


{-| The Take Element Screenshot command takes a screenshot of the visible region encompassed by the bounding rectangle of an element.
If given a parameter argument scroll that evaluates to false, the element will not be scrolled into view.
-}
elementScreenshot : WithSession -> Element -> Out String
elementScreenshot =
    requestElemntProp_ "screenshot" answerDecoder.string


{-| The Element Click command scrolls into view the element if it is not already pointer-interactable,
and clicks its in-view center point.

If the element’s center point is obscured by another element,
an element click intercepted error is returned. If the element is outside the viewport,
an element not interactable error is returned.

Other input way (touch events, mouse up/down ...) can be done by [`actions`](#actions)

-}
click : WithSession -> Element -> Out ()
click settings (Value.Element element) =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/element/" ++ element ++ "/click")
        Http.emptyBody
        answerDecoder.empty


{-| Clear a `TEXTAREA` or text `INPUT` element’s value.
-}
clear : WithSession -> Element -> Out ()
clear settings (Value.Element element) =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/element/" ++ element ++ "/clear")
        Http.emptyBody
        answerDecoder.empty


{-| Send a sequence of key strokes to an element.
You can also use unicode characters like `Left arrow` or `Backspace` ([`keys`](../WebDriver.Keyboard#key))
WebdriverIO will take care of translating them into unicode characters.
You’ll find all supported characters here. To do that, the value has to correspond to a key from the table.
-}
value : WithSession -> String -> Element -> Out ()
value settings value (Value.Element element) =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/element/" ++ element ++ "/value")
        (Json.object
            [ ( "value"
              , Json.list [ Json.string value ]
              )
            ]
            |> Http.jsonBody
        )
        answerDecoder.empty



-- POST	/session/{session id}/element/{element id}/value	Element Send Keys


{-| Change focus to the parent context.
If the current context is the top level browsing context,
the context remains unchanged.
-}
frameParent : WithSession -> Out ()
frameParent settings =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/frame/parent")
        Http.emptyBody
        answerDecoder.empty


{-|

> TODO improve arguments

Change focus to another frame on the page.
If the frame id is null, the server should switch to the page’s default content.

-}
frame : WithSession -> Json.Value -> Out ()
frame settings id =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/frame")
        (Json.object
            [ ( "id"
              , id
              )
            ]
            |> Http.jsonBody
        )
        answerDecoder.empty


{-| Get source code of the page. This command won’t work in mobile environments for native apps.
If you are running hybrid tests make sure that you are in the webview before calling this command.
-}
source : WithSession -> Out String
source settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/url")
        answerDecoder.string


{-| Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
The executed script is assumed to be synchronous and the result of evaluating the script is returned to the client.

The script argument defines the script to execute in the form of a function body.
The value returned by that function will be returned to the client.
The function will be invoked with the provided args `List Json.Value`
and the values may be accessed via the arguments object in the order specified.

Arguments may be any JSON-primitive, array, or JSON object.

    execute
        settings
        "alert(arguments[arguments.length - 1])"
        [ Json.string "4123412", Json.int 3 ]

-}
execute : WithSession -> String -> List Json.Value -> Out Json.Value
execute settings function args =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/execute/sync")
        (Json.object
            [ ( "script"
              , Json.string function
              )
            , ( "args"
              , Json.list args
              )
            ]
            |> Http.jsonBody
        )
        answerDecoder.value


{-| Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
The executed script is assumed to be asynchronous and must signal that is done by invoking the provided callback,
which is always provided as the final argument to the function.
The value to this callback will be returned to the client.

Asynchronous script commands may not span page loads.
If an unload event is fired while waiting for a script result,
an error should be returned to the client.

The script argument defines the script to execute in the form of a function body.
The function will be invoked with the provided args `List Json.Value`
and the values may be accessed via the arguments object in the order specified.
The final argument will always be a callback function that must be invoked to signal that the script has finished.

Arguments may be any JSON-primitive, array, or JSON object.

    let
        script =
            """
        const {a,b,c,d,done} = arguments;
        setTimeout(function() {
                done(a + b + c + d);
            }, 3000);"""
    in
    executeAsync
        settings
        script
        [ Json.string "4123412", Json.int 3 ]

-}
executeAsync : WithSession -> String -> List Json.Value -> Out Json.Value
executeAsync settings function args =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/execute/async")
        (Json.object
            [ ( "script"
              , Json.string function
              )
            , ( "args"
              , Json.list args
              )
            ]
            |> Http.jsonBody
        )
        answerDecoder.value



----COOKIES----


{-| Get All Cookies
-}
cookies : WithSession -> Out (List Cookie)
cookies settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/cookie")
        answerDecoder.cookies


{-| Get Named Cookie
-}
cookie : { a | sessionId : String, url : String } -> String -> Out Cookie
cookie settings name =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/cookie/" ++ name)
        answerDecoder.cookie


{-| Delete cookies visible to the current page.
-}
deleteCookies : WithSession -> Out ()
deleteCookies settings =
    Http.delete (settings.url ++ "/session/" ++ settings.sessionId ++ "/cookie")
        answerDecoder.empty


{-| Delete cookie visible to the current page.
By providing a cookie name.
-}
deleteCookie : WithSession -> String -> Out ()
deleteCookie settings name =
    Http.delete (settings.url ++ "/session/" ++ settings.sessionId ++ "/cookie/" ++ name)
        answerDecoder.empty


{-| Sets a cookie for current page.
Make sure you are on the page that should receive the cookie.
You can’t set a cookie for an arbitrary page without being on that page.
-}
addCookie : WithSession -> String -> String -> Out ()
addCookie settings name value =
    let
        data =
            Json.object
                [ ( "cookie"
                  , Json.object
                        [ ( "name", Json.string name )
                        , ( "value", Json.string value )
                        ]
                  )
                ]
    in
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/cookie")
        (data |> Http.jsonBody)
        answerDecoder.empty


{-| Accepts the currently displayed alert dialog. Usually, this is equivalent to clicking on the ‘OK’ button in the dialog.
-}
alertAccept : WithSession -> Out ()
alertAccept settings =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/alert/accept")
        Http.emptyBody
        answerDecoder.empty


{-| Dismisses the currently displayed alert dialog. For confirm() and prompt() dialogs, this is equivalent to clicking the ‘Cancel’ button. For alert() dialogs, this is equivalent to clicking the ‘OK’ button.
-}
alertDismiss : WithSession -> Out ()
alertDismiss settings =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/alert/dismiss")
        Http.emptyBody
        answerDecoder.empty


{-| Gets the text of the currently displayed JavaScript alert(), confirm(), or prompt() dialog.
-}
alertText : WithSession -> Out String
alertText settings =
    Http.get (settings.url ++ "/session/" ++ settings.sessionId ++ "/alert/text")
        answerDecoder.string


{-| Keystrokes to send to the prompt() dialog.
-}
promptText : WithSession -> String -> Out ()
promptText settings value =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/alert/text")
        (Json.object [ ( "text", Json.string value ) ] |> Http.jsonBody)
        answerDecoder.empty



---ACTIONS---


{-| Actions are a very complex portion of the spec. Some preliminary understanding of concepts is useful:

  - tick: a slice of an action chain. Actions from different input sources can be executed simultaneously.
    These are first lined up from the first action. Every vertical “slice” across the different input sources’ action lists is a tick.
    A tick is not associated with any particular time value, and lasts as long as the longest action duration inside the tick.
  - input source: a representation of an input device like a keyboard, mouse, finger, or pen. There can be any number of input sources. Each one has its own id.
  - action: a behavior performed by an input source. Different types of input source have different types of possible actions

The command takes a list of input source actions. In other words, a list of objects,
each of which represents an input source and its associated actions.
Each input source must have the following properties:

  - type: String, one of pointer, key, or none
  - id: String, a unique id chosen to represent this input source for this and future actions
  - parameters: pointer-type input sources can also have a parameters property, which is an object with a pointerType key specifying either mouse, pen, or touch. If parameters is omitted, the pointerType is considered to be mouse.
  - actions: a list of action objects for this particular input source. An action object has different fields based on the kind of input device it belongs to (see also [`here`](https://github.com/jlipps/simple-wd-spec#input-sources-and-corresponding-actions))

-}
actions : WithSession -> List Action -> Out ()
actions settings action =
    Http.post (settings.url ++ "/session/" ++ settings.sessionId ++ "/actions")
        (Json.object [ ( "actions", List.map Action.encode action |> Json.list ) ] |> Http.jsonBody)
        answerDecoder.empty


{-| The Release Actions command is used to release all the keys and pointer buttons that are currently depressed.
This causes events to be fired as if the state was released by an explicit series of actions.
It also clears all the internal state of the virtual devices.
-}
release : WithSession -> Out ()
release settings =
    Http.delete (settings.url ++ "/session/" ++ settings.sessionId ++ "/actions")
        answerDecoder.empty
