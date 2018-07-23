module WebDriver.Internal.Value.Action exposing (Action(..), encode)

import Json.Encode as Json


-- https://github.com/jlipps/simple-wd-spec#perform-actions


type Action
    = -- | PointerAction ActionData
      MouseAction (ActionData PointerActivity)
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


type Origin
    = OriginElement
    | OriginViewport
    | OriginPointer


encode : Action -> Json.Value
encode action =
    case action of
        MouseAction data ->
            Json.string "TODO"

        PenAction data ->
            Json.string "TODO"

        TouchAction data ->
            Json.string "TODO"

        KeyAction data ->
            Json.string "TODO"

        NoneAction data ->
            Json.string "TODO"


wrapper : Json.Value -> Json.Value
wrapper data =
    Json.object [ ( "actions", data ) ]


keyEncoder : String -> String -> List KeyActivity -> Json.Value
keyEncoder kind id actions =
    let
        encodeOne action =
            case action of
                KeyDown c ->
                    --  {"type": "keyDown", "value": "\uE009"},
                    Json.object [ ( "type", Json.string "keyDown" ), ( "value", c |> String.fromChar |> Json.string ) ]

                KeyUp c ->
                    --  {"type": "keyUp", "value": "\uE009"},
                    Json.object [ ( "type", Json.string "keyUp" ), ( "value", c |> String.fromChar |> Json.string ) ]

                KeyPause duration ->
                    -- {"type": "pause", "duration": 500},
                    Json.object [ ( "type", Json.string "pause" ), ( "duration", Json.int duration ) ]
    in
    Json.object
        [ ( "type", Json.string kind )
        , ( "id", Json.string id )
        , ( "actions", List.map encodeOne actions |> Json.list )
        ]
