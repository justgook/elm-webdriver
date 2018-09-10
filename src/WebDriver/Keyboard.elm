module WebDriver.Keyboard exposing (key)

{-|

@docs key

-}


{-| [`WebDriver documetation`](https://w3c.github.io/webdriver/#keyboard-actions).
-}
key :
    { alt : Char
    , alt2 : Char
    , arrowDown : Char
    , arrowDown2 : Char
    , arrowLeft : Char
    , arrowLeft2 : Char
    , arrowRight : Char
    , arrowRight2 : Char
    , arrowUp : Char
    , arrowUp2 : Char
    , backspace : Char
    , cancel : Char
    , clear : Char
    , control : Char
    , control2 : Char
    , delete : Char
    , delete2 : Char
    , end : Char
    , end2 : Char
    , enter : Char
    , escape : Char
    , f1 : Char
    , f10 : Char
    , f11 : Char
    , f12 : Char
    , f2 : Char
    , f3 : Char
    , f4 : Char
    , f5 : Char
    , f6 : Char
    , f7 : Char
    , f8 : Char
    , f9 : Char
    , help : Char
    , home : Char
    , home2 : Char
    , insert : Char
    , insert2 : Char
    , meta : Char
    , meta2 : Char
    , pageDown : Char
    , pageDown2 : Char
    , pageUp : Char
    , pageUp2 : Char
    , pause : Char
    , return : Char
    , shift : Char
    , shift2 : Char
    , space : Char
    , tab : Char
    , unidentified : Char
    , zenkakuHankaku : Char
    }
key =
    { unidentified = '\u{E000}'
    , cancel = '\u{E001}'
    , help = '\u{E002}'
    , backspace = '\u{E003}'
    , tab = '\u{E004}'
    , clear = '\u{E005}'
    , return = '\u{E006}'
    , enter = '\u{E007}'
    , shift = '\u{E008}'
    , control = '\u{E009}'
    , alt = '\u{E00A}'
    , pause = '\u{E00B}'
    , escape = '\u{E00C}'
    , space = '\u{E00D}'
    , pageUp = '\u{E00E}'
    , pageDown = '\u{E00F}'
    , end = '\u{E010}'
    , home = '\u{E011}'
    , arrowLeft = '\u{E012}'
    , arrowUp = '\u{E013}'
    , arrowRight = '\u{E014}'
    , arrowDown = '\u{E015}'
    , insert = '\u{E016}'
    , delete = '\u{E017}'
    , f1 = '\u{E031}'
    , f2 = '\u{E032}'
    , f3 = '\u{E033}'
    , f4 = '\u{E034}'
    , f5 = '\u{E035}'
    , f6 = '\u{E036}'
    , f7 = '\u{E037}'
    , f8 = '\u{E038}'
    , f9 = '\u{E039}'
    , f10 = '\u{E03A}'
    , f11 = '\u{E03B}'
    , f12 = '\u{E03C}'
    , meta = '\u{E03D}'
    , zenkakuHankaku = '\u{E040}'
    , shift2 = '\u{E050}'
    , control2 = '\u{E051}'
    , alt2 = '\u{E052}'
    , meta2 = '\u{E053}'
    , pageUp2 = '\u{E054}'
    , pageDown2 = '\u{E055}'
    , end2 = '\u{E056}'
    , home2 = '\u{E057}'
    , arrowLeft2 = '\u{E058}'
    , arrowUp2 = '\u{E059}'
    , arrowRight2 = '\u{E05A}'
    , arrowDown2 = '\u{E05B}'
    , insert2 = '\u{E05C}'
    , delete2 = '\u{E05D}'
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
