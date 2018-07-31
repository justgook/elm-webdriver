module NestedSet
    exposing
        ( NestedSet
        , depth
        , empty
        , get
        , indexedFoldr
        , insert
        , insertAfter
        , length
        , map
        , order
        , set
        , update
        )

{-| The nested set model is a particular technique for representing nested sets (also known as trees or hierarchies) in relational databases.

@docs NestedSet


# Creating Nested Set

@docs empty


# Basics

@docs length


# Get and Set


## Fast

@docs get, set, update, map, depth


## Slow

@docs insert, insertAfter, order


# Mapping, Filtering, and Folding

@docs map, indexedFoldr

-}

--`Nested set model`
-- https://www.we-rc.com/blog/2015/07/19/nested-set-model-practical-examples-part-i
--  http://mikehillyer.com/articles/managing-hierarchical-data-in-mysql/

import Array exposing (Array)


{-| Nested Set Model
-}
type NestedSet a
    = NestedSet { balanced : Bool, tree : Array (Node a) }


{-| Node representing record in tree
-}
type Node a
    = Node { lft : Int, rgt : Int, parent_id : Int, value : a }


{-| Return an empty Nested set.
-}
empty : NestedSet a
empty =
    NestedSet { balanced = True, tree = Array.empty }


{-| Inser element to to Tree under some parent index, or put `-1` to insert in top.
If the index is out of range, the Nested Set is unaltered.
-}
insert : Int -> a -> NestedSet a -> NestedSet a
insert index value ((NestedSet { tree }) as income) =
    let
        mapNode f (Node a) =
            f a

        parentNode =
            Array.get index tree

        outcome =
            case ( parentNode, Array.length tree ) of
                ( Nothing, 0 ) ->
                    --Insert first
                    { balanced = True
                    , tree =
                        Array.push
                            (Node
                                { lft = 1
                                , rgt = 2
                                , parent_id = -1
                                , value = value
                                }
                            )
                            tree
                    }
                        |> NestedSet

                ( Nothing, _ ) ->
                    if index == -1 then
                        case findLastIndexWhere (\(Node { parent_id }) -> parent_id == -1) tree of
                            Just lastInTopLevel ->
                                insertAfter lastInTopLevel value income

                            Nothing ->
                                Debug.log "Imposible State - make me imposible" income
                    else
                        income

                ( Just (Node { lft }), _ ) ->
                    case findLastIndexWhere (\(Node { parent_id }) -> parent_id == index) tree of
                        Just lastInLevel ->
                            insertAfter lastInLevel value income

                        Nothing ->
                            income
                                |> mapNode_ (insertMapper_ lft)
                                |> (\(NestedSet data) ->
                                        { balanced = False
                                        , tree =
                                            Array.push
                                                (Node { lft = lft + 1, rgt = lft + 2, parent_id = index, value = value })
                                                data.tree
                                        }
                                            |> NestedSet
                                   )
    in
    outcome


findLastIndexWhere : (a -> Bool) -> Array a -> Maybe Int
findLastIndexWhere predicate array =
    array
        |> Array.foldl (step predicate) ( -1, Nothing )
        |> Tuple.second


step : (a -> Bool) -> a -> ( Int, Maybe Int ) -> ( Int, Maybe Int )
step predicate item ( index, lastMatch ) =
    if predicate item then
        ( index + 1, Just (index + 1) )
    else
        ( index + 1, lastMatch )


{-| Insert element after provided index.
If the index is out of range, the Nested Set is unaltered.
-}
insertAfter : Int -> a -> NestedSet a -> NestedSet a
insertAfter index value ((NestedSet { tree }) as income) =
    case Array.get index tree of
        Just (Node { rgt, parent_id }) ->
            income
                |> mapNode_ (insertMapper_ rgt)
                |> (\(NestedSet data) ->
                        { balanced = False
                        , tree =
                            Array.push
                                (Node { lft = rgt + 1, rgt = rgt + 2, parent_id = parent_id, value = value })
                                data.tree
                        }
                            |> NestedSet
                   )

        Nothing ->
            income


insertMapper_ : Int -> { a | lft : Int, rgt : Int } -> { a | lft : Int, rgt : Int }
insertMapper_ input ({ rgt, lft } as data) =
    let
        applyIf condition fn value =
            if condition then
                fn value
            else
                value

        incLeft data =
            { data | lft = data.lft + 2 }

        incRight data =
            { data | rgt = data.rgt + 2 }
    in
    data
        |> applyIf (lft > input) incLeft
        |> applyIf (rgt > input) incRight


{-| Return node count in tree.
-}
length : NestedSet a -> Int
length (NestedSet { tree }) =
    Array.length tree


{-| Return Just the element at the index or Nothing if the index is out of range.
-}
get : Int -> NestedSet a -> Maybe a
get index (NestedSet { tree }) =
    Array.get index tree |> Maybe.map (\(Node item) -> item.value)


{-| Set the element at a particular index. Returns an updated tree.
If the index is out of range, the Nested Set is unaltered.
-}
set : Int -> a -> NestedSet a -> NestedSet a
set index value ((NestedSet ({ tree } as data)) as income) =
    Array.get index tree
        |> Maybe.map
            (\node ->
                NestedSet
                    { data
                        | tree = Array.set index (mapValue_ (always value) node) tree
                    }
            )
        |> Maybe.withDefault income


{-| Update the element at a particular index. Returns an updated tree.
If the index is out of range, the Nested Set is unaltered.
-}
update : Int -> (a -> a) -> NestedSet a -> NestedSet a
update index f data =
    case get index data of
        Just updatedData ->
            set index (f updatedData) data

        Nothing ->
            data


mapNode_ : ({ lft : Int, parent_id : Int, rgt : Int, value : a } -> { lft : Int, parent_id : Int, rgt : Int, value : a1 }) -> NestedSet a -> NestedSet a1
mapNode_ f (NestedSet data) =
    NestedSet { data | tree = Array.map (\(Node a) -> f a |> Node) data.tree }


mapValue_ : (a -> b) -> Node a -> Node b
mapValue_ f (Node data) =
    f data.value |> (\output -> Node { data | value = output })


{-| Apply a function on every element in an tree.
-}
map : (a -> b) -> NestedSet a -> NestedSet b
map f (NestedSet data) =
    NestedSet { data | tree = Array.map (mapValue_ f) data.tree }



-- {-| Apply a function on every elementn with its index as first argument.
-- -}
-- indexedMap : (Int -> a -> b) -> NestedSet a -> NestedSet b
-- indexedMap f (NestedSet input) =
--     let
--         indexedMapValue index (Node data) =
--             f index data.value |> (\output -> Node { data | value = output })
--     in
--     Array.indexedMap indexedMapValue input
--         |> NestedSet


{-| Reduce an Ordered Nested Set from the left.
-}
indexedFoldr : (Int -> a -> b -> b) -> b -> NestedSet a -> b
indexedFoldr f acc ((NestedSet { tree, balanced }) as income) =
    let
        foldValue f ( index, Node data ) acc_ =
            f index data.value acc_
    in
    tree
        |> Array.toIndexedList
        |> (if balanced then
                identity
            else
                List.sortBy (\( _, Node { lft } ) -> lft)
           )
        |> List.foldl (foldValue f) acc


{-| Reorder indexes of tree to match it position and increase speed of fold functions.
-}
order : NestedSet a -> NestedSet a
order ((NestedSet { tree, balanced }) as income) =
    if balanced then
        income
    else
        NestedSet
            { tree =
                tree
                    |> Array.toList
                    |> List.sortBy (\(Node { lft }) -> lft)
                    |> Array.fromList
            , balanced = True
            }


{-| Calculate depth of Node by it index.
If celement not found returns -1.
-}
depth : Int -> NestedSet a -> Int
depth =
    let
        depth_ current index ((NestedSet { tree }) as income) =
            case Array.get index tree of
                Just (Node { parent_id }) ->
                    depth_ (current + 1) parent_id income

                Nothing ->
                    current
    in
    depth_ -1
