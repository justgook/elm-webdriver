module Util.Tuple exposing (mapBoth)


mapBoth : (a -> x) -> (b -> y) -> ( a, b ) -> ( x, y )
mapBoth funcA funcB ( x, y ) =
    ( funcA x, funcB y )
