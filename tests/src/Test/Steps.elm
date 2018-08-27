module Test.Steps exposing (suite)

import Json.Decode as Decode
import Json.Encode as Json
import Task
import WebDriver as WebDriver exposing (describe, only, test)
import WebDriver.Assert as Assert
import WebDriver.Step.Element as Selector


hostForCookies : String
hostForCookies =
    "http://example.com"


suite : WebDriver.Test a
suite =
    describe "WebDriver"
        [ describe "Step"
            [ test "url" <|
                \{ url, getUrl, refresh } ->
                    url blankPage
            , test "getUrl" <|
                \{ url, getUrl, refresh } ->
                    url blankPage
                        |> Task.andThen (\_ -> getUrl)
                        |> Task.andThen (.value >> Assert.equal blankPage)
            , test "title" <|
                \{ url, title } ->
                    url tiltlePage
                        |> Task.andThen (\_ -> title)
                        |> Task.andThen
                            (.value
                                >> (\value ->
                                        String.contains "Hello" value
                                            |> Assert.true ("Expected the title contains text Hello, but it is `" ++ value ++ "`")
                                   )
                            )
            , test "refresh" <|
                \{ url, getUrl, refresh } ->
                    url blankPage
                        |> Task.andThen (\_ -> refresh)
                        |> Task.andThen (\_ -> getUrl)
                        |> Task.andThen (.value >> Assert.equal blankPage)
            , test "back" <|
                \{ url, getUrl, back, forward } ->
                    url blankPage
                        |> Task.andThen (\_ -> url blankPage2)
                        |> Task.andThen (\_ -> back)
                        |> Task.andThen (\_ -> getUrl)
                        |> Task.andThen (.value >> Assert.equal blankPage)
            , test "forward" <|
                \{ url, getUrl, back, forward } ->
                    url blankPage
                        |> Task.andThen (\_ -> back)
                        |> Task.andThen (\_ -> forward)
                        |> Task.andThen (\_ -> getUrl)
                        |> Task.andThen (.value >> Assert.equal blankPage)
            , test "element, text" <|
                \{ url, element, text } ->
                    url mock
                        |> Task.andThen (\_ -> "h1" |> Selector.css |> element)
                        |> Task.andThen (.value >> text)
                        |> Task.andThen (.value >> Assert.equal "Hello World")
            , test "elements" <|
                \{ url, elements, text } ->
                    url mock
                        |> Task.andThen (\_ -> "h1" |> Selector.css |> elements)
                        |> Task.andThen
                            (.value
                                >> (List.head
                                        >> Maybe.map (text >> Task.andThen (.value >> Assert.equal "Hello World"))
                                        >> Maybe.withDefault (Task.fail "no elemment found")
                                   )
                            )
            , test "selected" <|
                \{ url, selected, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']>#selected" |> Selector.css |> element)
                        |> Task.andThen (.value >> selected)
                        |> Task.andThen (.value >> Assert.true "Element not selected")
            , test "enabled" <|
                \{ url, enabled, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']>#disabled" |> Selector.css |> element)
                        |> Task.andThen (.value >> enabled)
                        |> Task.andThen (.value >> Assert.false "Element sould be not enabled")
            , test "tagName" <|
                \{ url, tagName, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']" |> Selector.css |> element)
                        |> Task.andThen (.value >> tagName)
                        |> Task.andThen (.value >> Assert.equal "select")
            , test "attribute" <|
                \{ url, attribute, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']" |> Selector.css |> element)
                        |> Task.andThen (.value >> attribute "name")
                        |> Task.andThen (.value >> Assert.equal "select1")
            , test "property" <|
                \{ url, property, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']" |> Selector.css |> element)
                        |> Task.andThen (.value >> property "name")
                        |> Task.andThen (.value >> Assert.equal "select1")
            , test "css" <|
                \{ url, css, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='text']" |> Selector.css |> element)
                        |> Task.andThen (.value >> css "display")
                        |> Task.andThen (.value >> Assert.equal "block")
            , test "elementInElement" <|
                \{ url, elementInElement, selected, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']" |> Selector.css |> element)
                        |> Task.andThen (.value >> ("#selected" |> Selector.css |> elementInElement))
                        |> Task.andThen (.value >> selected)
                        |> Task.andThen (.value >> Assert.true "Expect find selected element in select box")
            , test "elementsInElement" <|
                \{ url, elementsInElement, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='select1']" |> Selector.css |> element)
                        |> Task.andThen (.value >> ("option" |> Selector.css |> elementsInElement))
                        |> Task.andThen (.value >> List.length >> Assert.greaterThan 1)
            , test "setWindowRect, getWindowRect" <|
                \{ url, setWindowRect, getWindowRect, element } ->
                    url form
                        |> Task.andThen (\_ -> setWindowRect { height = 530, width = 532, x = 33, y = 34 })
                        |> Task.andThen (\_ -> getWindowRect)
                        |> Task.andThen
                            (.value
                                >> (\{ height, width, x, y } ->
                                        Assert.equalInt height 530
                                            |> Task.andThen (\_ -> Assert.equalInt width 532)
                                            |> Task.andThen (\_ -> Assert.equalInt x 33)
                                            |> Task.andThen (\_ -> Assert.equalInt y 34)
                                   )
                            )
            , test "rect" <|
                \{ url, rect, element } ->
                    url form
                        |> Task.andThen (\_ -> "[name='text']" |> Selector.css |> element)
                        |> Task.andThen (.value >> rect)
                        |> Task.andThen (.value >> (\{ width } -> Assert.equalInt width 50))
            , test "click" <|
                \{ url, click, element, selected } ->
                    url form
                        |> Task.andThen (\_ -> "[name='checkbox']" |> Selector.css |> element)
                        |> Task.andThen (.value >> click)
                        |> Task.andThen (\_ -> "[name='checkbox']" |> Selector.css |> element)
                        |> Task.andThen (.value >> selected)
                        |> Task.andThen (.value >> Assert.false "By Clicking on checked box, expected get unselected status")
            , test "clear, value" <|
                \{ url, clear, value, element, attribute } ->
                    url form
                        |> Task.andThen (\_ -> "[name='text']" |> Selector.css |> element)
                        |> Task.andThen
                            (.value
                                >> (\elm ->
                                        value "123" elm
                                            |> Task.andThen (\_ -> attribute "value" elm)
                                            |> Task.andThen (.value >> Assert.equal "123")
                                            |> Task.andThen (\_ -> clear elm)
                                            |> Task.andThen (\_ -> attribute "value" elm)
                                            |> Task.andThen (.value >> Assert.equal "")
                                   )
                            )
            , test "execute" <|
                \{ url, execute, executeAsync } ->
                    url form
                        |> Task.andThen (\_ -> executeAsync "window.delme = 'correct';" [])
                        |> Task.andThen (\_ -> execute "return window.delme" [])
                        |> Task.andThen
                            (.value
                                >> (Decode.decodeValue Decode.string
                                        >> Assert.custom Debug.toString (Ok "correct")
                                   )
                            )
            , test "executeAsync" <|
                \{ url, execute, executeAsync } ->
                    url form
                        |> Task.andThen
                            (\_ ->
                                let
                                    script =
                                        """
                                            const {a, b, c, d} = arguments;
                                            const done = arguments[arguments.length - 1];
                                            window.delme = 'fixme';
                                            setTimeout(function() {
                                                    window.delme = 'correct';
                                                    done(a + b + c + d);
                                                }, 3000);
                                        """
                                in
                                executeAsync script [ Json.string "4123412", Json.int 3 ]
                            )
                        |> Task.andThen (\_ -> execute "return window.delme" [])
                        |> Task.andThen
                            (.value
                                >> (Decode.decodeValue Decode.string
                                        >> Assert.custom Debug.toString (Ok "correct")
                                   )
                            )
            , test "addCookie, cookie" <|
                \{ url, addCookie, cookie } ->
                    url hostForCookies
                        |> Task.andThen (\_ -> addCookie "a" "1")
                        |> Task.andThen (\_ -> addCookie "b" "2")
                        |> Task.andThen (\_ -> cookie "a")
                        |> Task.andThen (.value >> .value >> Assert.equal "1")
            , test "cookies" <|
                \{ url, addCookie, cookies } ->
                    url hostForCookies
                        |> Task.andThen (\_ -> addCookie "a" "1")
                        |> Task.andThen (\_ -> addCookie "b" "2")
                        |> Task.andThen (\_ -> addCookie "c" "2")
                        |> Task.andThen (\_ -> cookies)
                        |> Task.andThen (.value >> List.length >> Assert.equalInt 3)
            , test "deleteCookies" <|
                \{ url, addCookie, deleteCookies, cookies } ->
                    url hostForCookies
                        |> Task.andThen (\_ -> addCookie "a" "1")
                        |> Task.andThen (\_ -> addCookie "b" "2")
                        |> Task.andThen (\_ -> addCookie "c" "2")
                        |> Task.andThen (\_ -> deleteCookies)
                        |> Task.andThen (\_ -> cookies)
                        |> Task.andThen (.value >> List.length >> Assert.equalInt 0)
            , test "deleteCookie" <|
                \{ url, addCookie, deleteCookie, cookies } ->
                    url hostForCookies
                        |> Task.andThen (\_ -> addCookie "a" "1")
                        |> Task.andThen (\_ -> addCookie "b" "2")
                        |> Task.andThen (\_ -> addCookie "c" "2")
                        |> Task.andThen (\_ -> deleteCookie "b")
                        |> Task.andThen (\_ -> cookies)
                        |> Task.andThen (.value >> List.length >> Assert.equalInt 2)
            , test "promptText" <|
                \{ url, promptText } ->
                    url promptMock
                        |> Task.andThen (\_ -> promptText "Hello back")
            , test "alertText" <|
                \{ url, alertText } ->
                    url alertMock
                        |> Task.andThen (\_ -> alertText)
                        |> Task.andThen (.value >> Assert.equal "Hello World")
            , test "alertDismiss" <|
                \{ url, alertDismiss } -> url alertMock |> Task.andThen (\_ -> alertDismiss)
            , test "alertAccept" <|
                \{ url, alertAccept } -> url alertMock |> Task.andThen (\_ -> alertAccept)
            , test "screenshot" <|
                \{ url, screenshot } -> url mock |> Task.andThen (\_ -> screenshot)
            , test "elementScreenshot" <|
                \{ url, element, elementScreenshot } ->
                    url mock
                        |> Task.andThen (\_ -> "h1" |> Selector.css |> element)
                        |> Task.andThen (.value >> elementScreenshot)
            , test "status" <|
                \{ url, status } ->
                    url mock |> Task.andThen (\_ -> status)
            , test "setTimeouts, getTimeouts" <|
                \{ url, getTimeouts, setTimeouts } ->
                    url mock
                        |> Task.andThen (\_ -> setTimeouts { implicit = 100, pageLoad = 100, script = 100 })
            , test "windowHandle, window" <|
                \{ url, windowHandle, window } ->
                    url mock
                        |> Task.andThen (\_ -> windowHandle)
                        |> Task.andThen (.value >> window)
            , test "windowHandles" <|
                \{ url, windowHandles } ->
                    url mock
                        |> Task.andThen (\_ -> windowHandles)
                        |> Task.andThen (.value >> List.length >> Assert.atLeast 1)
            , test "close" <|
                \{ url, windowHandles, close } ->
                    url newWindow
                        |> Task.andThen (\_ -> windowHandles)
                        |> Task.andThen (.value >> List.length >> Assert.equalInt 2)
                        |> Task.andThen (\_ -> close)
                        |> Task.andThen (\_ -> windowHandles)
                        |> Task.andThen (.value >> List.length >> Assert.equalInt 1)
            , test "fullscreen" <|
                \{ url, fullscreen } -> url mock |> Task.andThen (\_ -> fullscreen)
            , test "maximize" <|
                \{ url, maximize } -> url mock |> Task.andThen (\_ -> maximize)
            , test "minimize" <|
                \{ url, minimize } -> url mock |> Task.andThen (\_ -> minimize)
            , test "frameParent" <|
                \{ url, frameParent } -> url mock |> Task.andThen (\_ -> frameParent)
            , test "frame" <|
                \{ url, frame } -> url mock |> Task.andThen (\_ -> frame Json.null)
            , test "actions" <|
                \{ url, actions } -> url mock |> Task.andThen (\_ -> Task.fail "not implemented")
            , test "release" <|
                \{ url, release } -> url mock |> Task.andThen (\_ -> Task.fail "not implemented")
            ]
        ]


blankPage : String
blankPage =
    "data:text/plain,Hello"


blankPage2 : String
blankPage2 =
    "data:text/plain,Hello2"


newWindow : String
newWindow =
    "data:text/html,<script> window.open('" ++ blankPage ++ "', '_blank').focus()</script>"


tiltlePage : String
tiltlePage =
    """data:text/html,<title>Hello World</title>"""


mock : String
mock =
    """data:text/html,
    <h1>Hello World</h1>
    <h1>Hello World2</h1>
"""


promptMock : String
promptMock =
    """data:text/html,<script>prompt('Hello World');</script>"""


alertMock : String
alertMock =
    """data:text/html,<script>alert('Hello World');</script>"""


form : String
form =
    """data:text/html,
    <form>
        <input type='checkbox' name='checkbox' checked />
        <input type='text' name='text' style='display:block;width:50px' checked />
        <select name='select1'>
            <option value='1'>option 1</option>
            <option value='2' disabled id='disabled'>option 2</option>
            <option value='3' selected id='selected'>option 3</option>
            <option value='4'>option 4</option>
            <option value='5'>option 5</option>
        </select>
    </form>
"""
