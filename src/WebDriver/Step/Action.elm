module WebDriver.Step.Action exposing
    ( mouse, pen, touch, key, Origin(..)
    , PointerMapper
    , Action, encode
    )

{-|

  - Example 1 (expressing a 1-second pinch-and-zoom with a 500ms wait after the fingers first touch):

         {
         "actions": [
             {
             "type": "pointer",
             "id": "finger1",
             "parameters": {"pointerType": "touch"},
             "actions": [
                 {"type": "pointerMove", "duration": 0, "x": 100, "y": 100},
                 {"type": "pointerDown", "button": 0},
                 {"type": "pause", "duration": 500},
                 {"type": "pointerMove", "duration": 1000, "origin": "pointer", "x": -50, "y": 0},
                 {"type": "pointerUp", "button": 0}
             ]
             }, {
             "type": "pointer",
             "id": "finger2",
             "parameters": {"pointerType": "touch"},
             "actions": [
                 {"type": "pointerMove", "duration": 0, "x": 100, "y": 100},
                 {"type": "pointerDown", "button": 0},
                 {"type": "pause", "duration": 500},
                 {"type": "pointerMove", "duration": 1000, "origin": "pointer", "x": 50, "y": 0},
                 {"type": "pointerUp", "button": 0}
             ]
             }
         ]
         }

  - Example 2 (equivalent to typing CTRL+S and releasing the keys, though releasing would be better performed by a call to Release Actions):

         {
         "actions": [
             {
             "type": "key",
             "id": "keyboard",
             "actions": [
                 {"type": "keyDown", "value": "\uE009"},
                 {"type": "keyDown", "value": "s"},
                 {"type": "keyUp", "value": "\uE009"},
                 {"type": "keyUp", "value": "s"}
             ]
             }
         ]
         }


## Creators

@docs mouse, pen, touch, key, Origin

@docs PointerMapper


## Low Level

@docs Action, encode

-}

-- https://github.com/jlipps/simple-wd-spec#perform-actions

import Json.Decode as D
import Json.Encode as E
import WebDriver.Internal.Value as Value
import WebDriver.Step.Element exposing (Element)


{-| -}
type alias PointerMapper =
    { cancel : PointerActivity
    , down : Int -> PointerActivity
    , move :
        { duration : Int, origin : Origin, x : Int, y : Int }
        -> PointerActivity
    , pause : Int -> PointerActivity
    , up : Int -> PointerActivity
    }
    -> List PointerActivity


{-| -}
type alias KeyMapper =
    { down : Char -> KeyActivity
    , pause : Int -> KeyActivity
    , up : Char -> KeyActivity
    }
    -> List KeyActivity


{-| -}
mouse : String -> PointerMapper -> Action
mouse id mapper =
    pointer MouseAction id mapper


{-| -}
pen : String -> PointerMapper -> Action
pen id mapper =
    pointer PenAction id mapper


{-| -}
touch : String -> PointerMapper -> Action
touch id mapper =
    pointer TouchAction id mapper


pointer : (ActionData PointerActivity -> Action) -> String -> PointerMapper -> Action
pointer constructor id mapper =
    let
        down button =
            PointerDown { button = button }

        up button =
            PointerUp { button = button }

        move { duration, origin, x, y } =
            PointerMove { duration = duration, origin = origin, x = x, y = y }

        pause duration =
            PointerPause duration

        cancel =
            PointerCancel

        actions =
            mapper
                { down = down
                , up = up
                , move = move
                , pause = pause
                , cancel = cancel
                }
    in
    constructor { id = id, actions = actions }


{-| -}
key : String -> KeyMapper -> Action
key id mapper =
    let
        down button =
            KeyDown button

        up button =
            KeyUp button

        pause duration =
            KeyPause duration

        actions =
            mapper
                { down = down
                , up = up
                , pause = pause
                }
    in
    KeyAction { id = id, actions = actions }


{-| -}
type Action
    = MouseAction (ActionData PointerActivity)
    | PenAction (ActionData PointerActivity)
    | TouchAction (ActionData PointerActivity)
    | KeyAction (ActionData KeyActivity)
    | NoneAction (ActionData PauseActivity) --Pointlesss


type alias ActionData actions =
    { id : String
    , actions : List actions
    }


type PauseActivity
    = Pause {- duration -} Int


type PointerActivity
    = PointerDown { button : Int }
    | PointerUp { button : Int }
    | PointerMove { duration : Int, origin : Origin, x : Int, y : Int }
    | PointerPause {- duration -} Int
    | {- this action is not yet defined by the spec -} PointerCancel


type KeyActivity
    = KeyDown Char
    | KeyUp Char
    | KeyPause {- duration -} Int


{-| -}
type Origin
    = OriginElement Element
    | OriginViewport
    | OriginPointer


{-| -}
encode : Action -> E.Value
encode action =
    case action of
        MouseAction data ->
            pointerEncodeWrap "mouse" data

        PenAction data ->
            pointerEncodeWrap "pen" data

        TouchAction data ->
            pointerEncodeWrap "touch" data

        KeyAction { id, actions } ->
            E.object
                [ ( "type", E.string "key" )
                , ( "id", E.string id )
                , ( "actions", E.list encodeKeyActivity actions )
                ]

        NoneAction { id, actions } ->
            E.object
                [ ( "type", E.string "none" )
                , ( "id", E.string id )
                , ( "actions", E.list (\(Pause a) -> encodePause a) actions )
                ]


encodeKeyActivity : KeyActivity -> E.Value
encodeKeyActivity activity =
    case activity of
        KeyDown k ->
            -- {"type": "keyDown", "value": "s"}
            E.object [ ( "type", E.string "keyDown" ), ( "value", String.fromChar k |> E.string ) ]

        KeyUp k ->
            -- {"type": "keyUp", "value": "s"}
            E.object [ ( "type", E.string "keyUp" ), ( "value", String.fromChar k |> E.string ) ]

        KeyPause duration ->
            encodePause duration


pointerEncodeWrap : String -> ActionData PointerActivity -> E.Value
pointerEncodeWrap pointerType { id, actions } =
    E.object
        [ ( "type", E.string "pointer" )
        , ( "id", E.string id )
        , ( "parameters", E.object [ ( "pointerType", E.string pointerType ) ] )
        , ( "actions", E.list encodePointerActivity actions )
        ]


encodePointerActivity : PointerActivity -> E.Value
encodePointerActivity activity =
    case activity of
        PointerDown { button } ->
            -- {"type": "pointerDown", "button": 0},
            E.object [ ( "type", E.string "pointerDown" ), ( "button", E.int button ) ]

        PointerUp { button } ->
            -- {"type": "pointerUp", "button": 0}
            E.object [ ( "type", E.string "pointerUp" ), ( "button", E.int button ) ]

        PointerMove { duration, origin, x, y } ->
            -- {"type": "pointerMove", "duration": 1000, "origin": "pointer", "x": 50, "y": 0},
            E.object
                [ ( "type", E.string "pointerMove" )
                , ( "duration", E.int duration )
                , ( "origin", encodeOrigin origin )
                , ( "x", E.int x )
                , ( "y", E.int y )
                ]

        PointerPause duration ->
            -- {"type": "pause", "duration": 500},
            encodePause duration

        PointerCancel ->
            E.null


encodePause : Int -> E.Value
encodePause duration =
    E.object [ ( "type", E.string "pause" ), ( "duration", E.int duration ) ]


encodeOrigin : Origin -> E.Value
encodeOrigin o =
    case o of
        OriginElement (Value.Element e) ->
            E.string e

        OriginViewport ->
            E.string "viewport"

        OriginPointer ->
            E.string "pointer"
