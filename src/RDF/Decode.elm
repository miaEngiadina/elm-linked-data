module RDF.Decode exposing
    ( Decoder
    , andThen
    , apply
    , decode
    , decodeAll
    , ensureType
    , fail
    , first
    , ignore
    , iriDecoder
    , literalDecoder
    , map
    , map2
    , objectsDecoder
    , sequence
    , succeed
    )

{-| Decode statically typed Elm values from an RDF graph.
-}

import RDF



-- RDF Decoder
-- This needs to be part of this module in order to be able to access internal representation of the graph.


type alias State =
    { graph : RDF.Graph
    , node : RDF.Node
    }


type alias Error =
    String


{-| A RDF decoder.
-}
type Decoder a
    = Decoder (State -> Result Error ( State, a ))


{-| Run the decoder.
-}
decode : Decoder a -> RDF.Graph -> RDF.Subject -> Result Error a
decode (Decoder f) graph node =
    f (State graph (RDF.asNode node))
        |> Result.map Tuple.second


{-| Run decoder on all subjects in Graph and return all matches.
-}
decodeAll : Decoder a -> RDF.Graph -> List a
decodeAll decoder graph =
    graph
        |> RDF.graphFilterMap
            (\triple_ ->
                decode decoder graph triple_.subject
                    |> Result.toMaybe
            )



-- Primitives


{-| Decoder that succeeds without doing anything.
-}
succeed : a -> Decoder a
succeed a =
    Decoder (\state -> Ok ( state, a ))


{-| Decoder that always fails.
-}
fail : String -> Decoder a
fail msg =
    Decoder (\_ -> Err msg)


{-| Run a decoder and then run another decoder.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder decoderA) =
    Decoder
        (\state1 ->
            decoderA state1
                |> Result.andThen
                    (\( state2, a ) ->
                        let
                            (Decoder decoderB) =
                                f a
                        in
                        decoderB state2
                    )
        )


{-| Transform result of a decoder
-}
map : (a -> b) -> Decoder a -> Decoder b
map f =
    andThen (f >> succeed)


{-| Combine the result from two decoders.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f decoderA decoderB =
    decoderA
        |> andThen
            (\a ->
                decoderB
                    |> map (f a)
            )


{-| Apply the result of a decoder to a decoder.
-}
apply : Decoder a -> Decoder (a -> b) -> Decoder b
apply decoderA decoderF =
    map2 (\f a -> f a) decoderF decoderA


{-| Run a decoder and ignore thre result.
-}
ignore : Decoder b -> Decoder a -> Decoder a
ignore decoderB decoderA =
    map2 (\a _ -> a) decoderA decoderB


{-| Sequence a list of decoders.
-}
sequence : List (Decoder a) -> Decoder (List a)
sequence decoders =
    case decoders of
        [] ->
            succeed []

        headDecoder :: tailDecoders ->
            headDecoder
                |> andThen
                    (\headValue ->
                        sequence tailDecoders
                            |> map (\tailValues -> headValue :: tailValues)
                    )


{-| Get the first successfully decoded value.
-}
first : Decoder (List a) -> Decoder a
first decoder =
    decoder
        |> andThen
            (\lst ->
                case lst of
                    [] ->
                        fail "Can not get first element from empty list."

                    fst :: _ ->
                        succeed fst
            )



-- Decode RDF components


{-| Decode and IRI
-}
iriDecoder : Decoder RDF.IRI
iriDecoder =
    Decoder
        (\state ->
            case RDF.asIRI state.node of
                Just iri ->
                    Ok ( state, iri )

                _ ->
                    Err "expecting iri"
        )


{-| Decode a Literal
-}
literalDecoder : Decoder RDF.Literal
literalDecoder =
    Decoder
        (\state ->
            case RDF.asLiteral state.node of
                Just literal ->
                    Ok ( state, literal )

                _ ->
                    Err "expecting literal"
        )


{-| Decode objects that are linked to current subject via predicate.
-}
objectsDecoder : RDF.Predicate -> Decoder a -> Decoder (List a)
objectsDecoder p (Decoder od) =
    Decoder
        (\state ->
            let
                decodeObjects object =
                    od { state | node = RDF.asNode object }
            in
            case RDF.asSubject state.node of
                Just subject ->
                    RDF.graphGetObjects state.graph subject p
                        |> List.map decodeObjects
                        |> List.filterMap Result.toMaybe
                        |> List.map Tuple.second
                        |> Tuple.pair state
                        |> Ok

                _ ->
                    Err "expecting IRI or BlankNode, got Literal"
        )


{-| Succeeds if current subject has type `iri`.
-}
ensureType : RDF.IRI -> Decoder ()
ensureType iri =
    objectsDecoder RDF.type_
        (iriDecoder
            |> andThen
                (\typeIRI ->
                    if typeIRI == iri then
                        succeed iri

                    else
                        fail "iri does not match"
                )
        )
        |> andThen
            (\matchingTypes ->
                if List.isEmpty matchingTypes then
                    fail "not of type"

                else
                    succeed ()
            )
