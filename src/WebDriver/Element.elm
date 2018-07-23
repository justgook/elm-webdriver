module WebDriver.Element exposing (Element, Selector, css, xPath)

{-| #Helper functions
@docs css, xPath

#Types
@docs Element, Selector

-}

import WebDriver.Internal.Value as Value


{-| -}
type alias Element =
    Value.Element


{-| -}
type alias Selector =
    Value.Selector


{-| Creates CSS selector to search for element in webpage
-}
css : String -> Selector
css query =
    Value.CSSselector query


{-| Creates XPath Selector to search for element in webpage
-}
xPath : String -> Value.Selector
xPath query =
    Value.XPat query
