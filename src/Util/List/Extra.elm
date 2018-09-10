module Util.List.Extra exposing (count, getAt, getFirst, indexedFoldl, indexedMapAccuml, mapAccuml, mapAccumr, updateAt)


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list

    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                head ++ fn x :: xs

            _ ->
                list


count : (a -> Bool) -> List a -> Int
count predicate =
    List.foldl
        (\x acc ->
            if predicate x then
                acc + 1

            else
                acc
        )
        0


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


mapAccuml : (a -> b -> ( a, c )) -> a -> List b -> ( a, List c )
mapAccuml f acc0 list =
    let
        ( accFinal, generatedList ) =
            List.foldl
                (\x ( acc1, ys ) ->
                    let
                        ( acc2, y ) =
                            f acc1 x
                    in
                    ( acc2, y :: ys )
                )
                ( acc0, [] )
                list
    in
    ( accFinal, List.reverse generatedList )


mapAccumr : (a -> b -> ( a, c )) -> a -> List b -> ( a, List c )
mapAccumr f acc0 list =
    List.foldr
        (\x ( acc1, ys ) ->
            let
                ( acc2, y ) =
                    f acc1 x
            in
            ( acc2, y :: ys )
        )
        ( acc0, [] )
        list


indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i + 1, func i x thisAcc )
    in
    Tuple.second (List.foldl step ( 0, acc ) list)


indexedFoldr : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldr func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i - 1, func i x thisAcc )
    in
    Tuple.second (List.foldr step ( List.length list - 1, acc ) list)


indexedMapAccuml : (Int -> a -> b -> ( a, c )) -> a -> List b -> ( a, List c )
indexedMapAccuml f acc0 list =
    let
        ( accFinal, generatedList ) =
            indexedFoldl
                (\i x ( acc1, ys ) ->
                    let
                        ( acc2, y ) =
                            f i acc1 x
                    in
                    ( acc2, y :: ys )
                )
                ( acc0, [] )
                list
    in
    ( accFinal, List.reverse generatedList )


getFirst : (a -> Bool) -> List a -> Maybe a
getFirst validation =
    let
        getFirst_ index list =
            case list of
                [] ->
                    Nothing

                x :: [] ->
                    if validation x then
                        Just x

                    else
                        Nothing

                x :: xs ->
                    if validation x then
                        Just x

                    else
                        getFirst_ (index + 1) xs
    in
    getFirst_ 0
