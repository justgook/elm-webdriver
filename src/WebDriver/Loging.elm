port module WebDriver.Loging exposing (log, success, info, error, skip, skipBadge, successBadge)


port log : String -> Cmd msg


success : String -> Cmd msg
success s =
    log (colors.fg.green ++ s ++ colors.reset)


info : String -> Cmd msg
info s =
    log (colors.fg.cyan ++ s ++ colors.reset)


error : String -> Cmd msg
error s =
    log (colors.fg.red ++ s ++ colors.reset)


skip : String -> Cmd msg
skip s =
    log (colors.fg.yellow ++ s ++ colors.reset)


skipBadge : String -> Cmd msg
skipBadge offset =
    log (offset ++ colors.bg.green ++ colors.fg.black ++ "Skip" ++ colors.reset)


successBadge : String -> Cmd msg
successBadge offset =
    log (offset ++ colors.fg.green ++ colors.bright ++ "Suuccess" ++ colors.reset)


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
