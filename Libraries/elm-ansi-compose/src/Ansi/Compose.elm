module Ansi.Compose
    exposing
        ( AnsiNode
        , Style
        , background
        , blink
        , bold
        , div
        , faint
        , foreground
        , fraktur
        , framed
        , inverted
        , italic
          -- , li
        , renderWindow
        , span
        , text
        , toModel
          -- , ul
        , underline
        )

import Ansi exposing (Color(..))
import Ansi.Log
import Array


type AnsiNode
    = Chunk { style : List Style, text : String } --Ansi.Log.Chunk
    | List (List Style) (List AnsiNode)
    | ListItem (List Style) { offset : Int, chunks : List AnsiNode }


type Style
    = Foreground Color
    | Background Color
    | Bold
    | Faint
    | Italic
    | Underline
    | Blink
    | Inverted
    | Fraktur
    | Framed


foreground : Color -> Style
foreground color =
    Foreground color


background : Color -> Style
background color =
    Background color


bold : Style
bold =
    Bold


faint : Style
faint =
    Faint


italic : Style
italic =
    Italic


underline : Style
underline =
    Underline


blink : Style
blink =
    Blink


inverted : Style
inverted =
    Inverted


fraktur : Style
fraktur =
    Fraktur


framed : Style
framed =
    Framed


type alias Color =
    Ansi.Color


ansiStylefromList : List Style -> Ansi.Log.Style
ansiStylefromList input =
    List.foldl
        (\item acc ->
            case item of
                Foreground c ->
                    { acc | foreground = Just c }

                Background c ->
                    { acc | background = Just c }

                Bold ->
                    { acc | bold = True }

                Faint ->
                    { acc | faint = True }

                Italic ->
                    { acc | italic = True }

                Underline ->
                    { acc | underline = True }

                Blink ->
                    { acc | blink = True }

                Inverted ->
                    { acc | inverted = True }

                Fraktur ->
                    { acc | fraktur = True }

                Framed ->
                    { acc | framed = True }
        )
        { foreground = Nothing
        , background = Nothing
        , bold = False
        , faint = False
        , italic = False
        , underline = False
        , blink = False
        , inverted = False
        , fraktur = False
        , framed = False
        }
        input


toModel : List AnsiNode -> Ansi.Log.Model -> Ansi.Log.Model
toModel items model =
    let
        result =
            List.foldr (\item acc -> Array.push ( item, Array.length acc ) acc) Array.empty (lines items)
    in
    { model | lines = result }


lines : List AnsiNode -> List (List Ansi.Log.Chunk)
lines items =
    let
        lines_ items_ offset_ parentStyle =
            items_
                |> List.foldl
                    (\item ( acc, remainder ) ->
                        case item of
                            Chunk ({ style, text } as chunk) ->
                                ( acc
                                , { chunk
                                    | style = ansiStylefromList (parentStyle ++ style)
                                    , text = String.repeat offset_ " " ++ chunk.text
                                  }
                                    :: remainder
                                )

                            List style subitems ->
                                let
                                    ( newAcc, newRemainder ) =
                                        lines_ subitems offset_ (parentStyle ++ style)
                                in
                                ( newAcc ++ acc, remainder )

                            ListItem style { offset, chunks } ->
                                let
                                    ( newAcc, newRemainder ) =
                                        lines_ chunks offset (parentStyle ++ style)
                                in
                                ( newAcc ++ [ newRemainder ] ++ acc, remainder )
                    )
                    ( [], [] )

        ( a, b ) =
            lines_ items 0 []
    in
    a ++ [ b ]


text : String -> AnsiNode
text t =
    Chunk
        { text = t
        , style = []
        }


span : List Style -> String -> AnsiNode
span style text =
    Chunk
        { text = text
        , style = style
        }


div : List Style -> List AnsiNode -> AnsiNode
div style items =
    List style items



-- p : List Style -> String -> AnsiNode
-- p style text =
--     Chunk
--         { text = text ++ "\n"
--         , style = style
--         }
-- ul : List Style -> List AnsiNode -> AnsiNode
-- ul style items =
--     let
--         update_offsets deep is =
--             is
--                 |> List.map
--                     (\item ->
--                         case item of
--                             Chunk _ ->
--                                 item
--                             List style subitems ->
--                                 List style (update_offsets deep subitems)
--                             ListItem style { offset, chunks } ->
--                                 ListItem style { offset = offset + deep, chunks = update_offsets deep chunks }
--                     )
--     in
--     items
--         |> update_offsets 2
--         |> List style
-- li : List Style -> List AnsiNode -> AnsiNode
-- li style chunks =
--     ListItem style { offset = 0, chunks = chunks }


renderWindow : Ansi.Log.Model -> String
renderWindow window =
    String.join "\x0D\n" (Array.toList (Array.map renderLine window.lines))


renderLine : Ansi.Log.Line -> String
renderLine ( chunks, _ ) =
    String.join "" (List.foldl (\c l -> renderChunk c :: l) [] chunks)


renderChunk : Ansi.Log.Chunk -> String
renderChunk chunk =
    "\x1B[0m" ++ styleFlags chunk.style ++ chunk.text


styleFlags : Ansi.Log.Style -> String
styleFlags style =
    String.join ""
        [ case style.foreground of
            Nothing ->
                ""

            Just fg ->
                "\x1B[" ++ toString (30 + colorCode fg) ++ "m"
        , case style.background of
            Nothing ->
                ""

            Just bg ->
                "\x1B[" ++ toString (40 + colorCode bg) ++ "m"
        , if style.bold then
            "\x1B[1m"
          else
            ""
        , if style.faint then
            "\x1B[2m"
          else
            ""
        , if style.italic then
            "\x1B[3m"
          else
            ""
        , if style.underline then
            "\x1B[4m"
          else
            ""
        , if style.blink then
            "\x1B[5m"
          else
            ""
        , if style.inverted then
            "\x1B[7m"
          else
            ""
        ]


colorCode : Ansi.Color -> Int
colorCode color =
    case color of
        Ansi.Black ->
            0

        Ansi.Red ->
            1

        Ansi.Green ->
            2

        Ansi.Yellow ->
            3

        Ansi.Blue ->
            4

        Ansi.Magenta ->
            5

        Ansi.Cyan ->
            6

        Ansi.White ->
            7

        Ansi.BrightBlack ->
            60

        Ansi.BrightRed ->
            61

        Ansi.BrightGreen ->
            62

        Ansi.BrightYellow ->
            63

        Ansi.BrightBlue ->
            64

        Ansi.BrightMagenta ->
            65

        Ansi.BrightCyan ->
            66

        Ansi.BrightWhite ->
            67
