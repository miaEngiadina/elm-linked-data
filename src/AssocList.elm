module AssocList exposing
    ( AssocList
    , get
    , keys
    , remove
    )

{-| Association List helpers
-}

import List.Extra


type alias AssocList a =
    List ( String, a )


{-| Get element with specified key from an association list
-}
get : String -> AssocList a -> Maybe a
get key assocList =
    List.Extra.find (\( k, v ) -> k == key) assocList
        |> Maybe.map Tuple.second


{-| Remove entries with given key
-}
remove : String -> AssocList a -> AssocList a
remove key assocList =
    List.filter (\( k, _ ) -> k /= key) assocList


{-| Return list of keys in association list
-}
keys : AssocList a -> List String
keys =
    List.map Tuple.first
