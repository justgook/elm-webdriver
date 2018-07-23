module WebDriver.Keyboard exposing (key)

{-|

@docs key

-}


{-| [`WebDriver documetation`](https://w3c.github.io/webdriver/#keyboard-actions).
-}
key : { alt : Char, alt2 : Char, arrowDown : Char, arrowDown2 : Char, arrowLeft : Char, arrowLeft2 : Char, arrowRight : Char, arrowRight2 : Char, arrowUp : Char, arrowUp2 : Char, backspace : Char, cancel : Char, clear : Char, control : Char, control2 : Char, delete : Char, delete2 : Char, end : Char, end2 : Char, enter : Char, escape : Char, f1 : Char, f10 : Char, f11 : Char, f12 : Char, f2 : Char, f3 : Char, f4 : Char, f5 : Char, f6 : Char, f7 : Char, f8 : Char, f9 : Char, help : Char, home : Char, home2 : Char, insert : Char, insert2 : Char, meta : Char, meta2 : Char, pageDown : Char, pageDown2 : Char, pageUp : Char, pageUp2 : Char, pause : Char, return : Char, shift : Char, shift2 : Char, space : Char, tab : Char, unidentified : Char, zenkakuHankaku : Char }
key =
    { unidentified = '\xE000'
    , cancel = '\xE001'
    , help = '\xE002'
    , backspace = '\xE003'
    , tab = '\xE004'
    , clear = '\xE005'
    , return = '\xE006'
    , enter = '\xE007'
    , shift = '\xE008'
    , control = '\xE009'
    , alt = '\xE00A'
    , pause = '\xE00B'
    , escape = '\xE00C'
    , space = '\xE00D'
    , pageUp = '\xE00E'
    , pageDown = '\xE00F'
    , end = '\xE010'
    , home = '\xE011'
    , arrowLeft = '\xE012'
    , arrowUp = '\xE013'
    , arrowRight = '\xE014'
    , arrowDown = '\xE015'
    , insert = '\xE016'
    , delete = '\xE017'
    , f1 = '\xE031'
    , f2 = '\xE032'
    , f3 = '\xE033'
    , f4 = '\xE034'
    , f5 = '\xE035'
    , f6 = '\xE036'
    , f7 = '\xE037'
    , f8 = '\xE038'
    , f9 = '\xE039'
    , f10 = '\xE03A'
    , f11 = '\xE03B'
    , f12 = '\xE03C'
    , meta = '\xE03D'
    , zenkakuHankaku = '\xE040'
    , shift2 = '\xE050'
    , control2 = '\xE051'
    , alt2 = '\xE052'
    , meta2 = '\xE053'
    , pageUp2 = '\xE054'
    , pageDown2 = '\xE055'
    , end2 = '\xE056'
    , home2 = '\xE057'
    , arrowLeft2 = '\xE058'
    , arrowUp2 = '\xE059'
    , arrowRight2 = '\xE05A'
    , arrowDown2 = '\xE05B'
    , insert2 = '\xE05C'
    , delete2 = '\xE05D'
    }



-- ;= '\xE018',
-- = = '\xE019',
-- 0 = '\xE01A',
-- 1 = '\xE01B',
-- 2 = '\xE01C',
-- 3 = '\xE01D',
-- 4 = '\xE01E',
-- 5 = '\xE01F',
-- 6 = '\xE020',
-- 7 = '\xE021',
-- 8 = '\xE022',
-- 9 = '\xE023',
-- *= '\xE024',
-- += '\xE025',
--,= '\xE026',
-- -= '\xE027',
-- .= '\xE028',
-- / = '\xE029',
