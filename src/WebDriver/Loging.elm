port module WebDriver.Loging exposing (error, info, log, skip, skipBadge, success, successBadge)


port log : String -> Cmd msg



-- http://ascii-table.com/ansi-escape-sequences.php


cHECK_MARK =
    "\\033[0;32m\\xE2\\x9C\\x94\\033[0m"



-- CHECK_MARK="\033[0;32m\xE2\x9C\x94\033[0m"
-- to clear from the cursor position to the beginning of the line:
-- echo -e "\033[1K"
-- Or everything on the line, regardless of cursor position:
-- echo -e "\033[2K"


success : String -> Cmd msg
success s =
    log (colors.fg.green ++ s ++ colors.reset ++ "\n")


info : String -> Cmd msg
info s =
    log (colors.fg.cyan ++ s ++ colors.reset ++ "\n")


error : String -> Cmd msg
error s =
    log (colors.fg.red ++ "✕" ++ s ++ colors.reset ++ "\n")


skip : String -> Cmd msg
skip s =
    log (colors.fg.yellow ++ "○" ++ s ++ colors.reset ++ "\n")


skipBadge : String -> Cmd msg
skipBadge offset =
    log (colors.fg.yellow ++ "○" ++ offset ++ colors.bg.green ++ colors.fg.black ++ "Skip" ++ colors.reset ++ "\n")


successBadge : String -> Cmd msg
successBadge offset =
    log (colors.fg.green ++ "✓" ++ offset ++ colors.bright ++ "Pass" ++ colors.reset ++ "\n")



-- https://gist.github.com/KenanSulayman/4990953
-- process.stdout.write('\033c\033[3J');


colors : Colors
colors =
    { reset = "\x1B[0m"
    , bright = "\x1B[1m"
    , dim = "\x1B[2m"
    , underscore = "\x1B[4m"
    , blink = "\x1B[5m"
    , reverse = "\x1B[7m"
    , hidden = "\x1B[8m"
    , fg =
        { black = "\x1B[30m"
        , red = "\x1B[31m"
        , green = "\x1B[32m"
        , yellow = "\x1B[33m"
        , blue = "\x1B[34m"
        , magenta = "\x1B[35m"
        , cyan = "\x1B[36m"
        , white = "\x1B[37m"
        , crimson = "\x1B[38m"
        }
    , bg =
        { black = "\x1B[40m"
        , red = "\x1B[41m"
        , green = "\x1B[42m"
        , yellow = "\x1B[43m"
        , blue = "\x1B[44m"
        , magenta = "\x1B[45m"
        , cyan = "\x1B[46m"
        , white = "\x1B[47m"
        , crimson = "\x1B[48m"
        }
    }


type alias Colors =
    { bg : { black : String, blue : String, crimson : String, cyan : String, green : String, magenta : String, red : String, white : String, yellow : String }, blink : String, bright : String, dim : String, fg : { black : String, blue : String, crimson : String, cyan : String, green : String, magenta : String, red : String, white : String, yellow : String }, hidden : String, reset : String, reverse : String, underscore : String }
