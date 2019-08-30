module Spaghetti exposing
    ( State(..)
    , andMap
    , andThen
    , continue
    , done
    , fail
    , ifAndThen
    , join
    , map
    , map2
    , mapError
    , maybeAndThen
    , pure
    , toResult
    )

{-| Helpers for implementing RFC style spaghetti code algorithms
-}


{-| Handle state of algorithm that may be either processing a value, in an error state or done with result.
-}
type State error value result
    = Processing value
    | Error error
    | Done result



-- Creators


{-| Algorithm is done with result
-}
done : result -> State error value result
done result =
    Done result


{-| Continue algorithm with state value
-}
continue : value -> State error value result
continue =
    pure


{-| Fail with error
-}
fail : error -> State error value result
fail error =
    Error error



-- Monad


pure : value -> State error value result
pure value =
    Processing value


andThen : (a -> State error b result) -> State error a result -> State error b result
andThen f x =
    case x of
        Processing v ->
            f v

        Error err ->
            Error err

        Done result ->
            Done result


join : State error (State error a result) result -> State error a result
join mma =
    mma
        |> andThen identity


{-| Sequence a computation if given function evaluates to Just.
-}
maybeAndThen : (a -> Maybe b) -> (b -> a -> State error a result) -> State error a result -> State error a result
maybeAndThen get f state =
    map (get >> Maybe.map f >> Maybe.withDefault pure) state
        |> andMap state
        |> join


{-| Conditionally sequence a computation
-}
ifAndThen : (a -> Bool) -> (a -> State error a result) -> State error a result -> State error a result
ifAndThen cond f state =
    andThen
        (\a ->
            if cond a then
                f a

            else
                pure a
        )
        state



-- Applicative


andMap : State error a result -> State error (a -> b) result -> State error b result
andMap x f =
    map2 (\f_ a -> f_ a) f x



-- Functor


map : (a -> b) -> State error a result -> State error b result
map f =
    andThen (f >> pure)


map2 : (a -> b -> c) -> State error a result -> State error b result -> State error c result
map2 f a b =
    a
        |> andThen (\a_ -> b |> map (f a_))


mapError : (errorA -> errorB) -> State errorA value result -> State errorB value result
mapError f x =
    case x of
        Error a ->
            Error (f a)

        Done result ->
            Done result

        Processing value ->
            Processing value



-- Result


{-| Convert state to result.
-}
toResult : (value -> Result error result) -> State error value result -> Result error result
toResult valueMapping state =
    case state of
        Done result ->
            Ok result

        Error error ->
            Err error

        Processing value ->
            valueMapping value
