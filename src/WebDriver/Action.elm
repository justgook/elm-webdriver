module WebDriver.Action exposing (Action)

{-|

@docs Action

-}

import WebDriver.Internal.Value.Action as Internal


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

-}
type alias Action =
    Internal.Action
