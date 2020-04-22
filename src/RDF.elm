module RDF exposing
    ( BlankNode
    , Decoder
    , Description
    , Graph
    , IRI
    , Literal
    , Object
    , Predicate
    , Subject
    , Triple
    , andThen
    , apply
    , asIRI
    , asLiteral
    , blankNode
    , decode
    , decodeAll
    , descriptionGet
    , ensureType
    , fail
    , filterForDescriptions
    , first
    , ignore
    , iri
    , iriDecoder
    , literal
    , literalDecoder
    , map
    , map2
    , namespace
    , objectBlankNode
    , objectIRI
    , objectLiteral
    , objectsDecoder
    , owl
    , predicateIRI
    , rdf
    , rdfs
    , sequence
    , subjectBlankNode
    , subjectIRI
    , succeed
    , triple
    , type_
    , xsd
    )

{-| Resource Description Framework (RDF). See <https://www.w3.org/TR/rdf11-concepts/>
-}

-- RDF Terms


{-| An Internationalized Resource Identifier. See <https://www.ietf.org/rfc/rfc3987.txt>.
-}
type alias IRI =
    String


{-| Create an IRI
-}
iri : String -> IRI
iri =
    identity


{-| A blank node
-}
type BlankNode
    = BNode String


{-| Create a new blank node.
-}
blankNode : String -> BlankNode
blankNode id =
    BNode id


{-| An RDF Literal
-}
type alias Literal =
    { value : String
    , datatype : IRI
    , language : Maybe String
    }


{-| Create a new literal
-}
literal : String -> IRI -> Maybe String -> Literal
literal value datatype maybeLanguage =
    Literal value datatype maybeLanguage



-- RDF Components


{-| Empty type for Subject component
-}
type CSubject
    = CSubject


{-| Empty type for Predicate component
-}
type CPredicate
    = CPredicate


{-| Empty type for Object component
-}
type CObject
    = CObject


{-| Internal type for nodes. This is a phantom type.
-}
type Node a
    = NodeIRI IRI
    | NodeBlankNode BlankNode
    | NodeLiteral Literal


{-| Cast a Subject, Predicate or Object back to an IRI
-}
asIRI : Node a -> Maybe IRI
asIRI node =
    case node of
        NodeIRI iri_ ->
            Just iri_

        _ ->
            Nothing


{-| Cast an Object back to an IRI
-}
asLiteral : Node a -> Maybe Literal
asLiteral node =
    case node of
        NodeLiteral literal_ ->
            Just literal_

        _ ->
            Nothing



-- Subject


{-| Subject which is an IRI or a blank node
-}
type alias Subject =
    Node CSubject


{-| Create a subject from an IRI
-}
subjectIRI : IRI -> Subject
subjectIRI i =
    NodeIRI i


{-| Create a subject from a blank node
-}
subjectBlankNode : BlankNode -> Subject
subjectBlankNode bnode =
    NodeBlankNode bnode



-- Predicate


{-| Predicate which is an IRI
-}
type alias Predicate =
    Node CPredicate


{-| Create a predicate from an IRI
-}
predicateIRI : IRI -> Predicate
predicateIRI i =
    NodeIRI i



-- Object


{-| Objects which can be a IRI, a blank node or a literal
-}
type alias Object =
    Node CObject


objectIRI : IRI -> Object
objectIRI i =
    NodeIRI i


objectBlankNode : BlankNode -> Object
objectBlankNode bnode =
    NodeBlankNode bnode


objectLiteral : Literal -> Object
objectLiteral l =
    NodeLiteral l


{-| The price for "no runtime-errors"
-}
rewrapNode : Node a -> Node b
rewrapNode node =
    case node of
        NodeIRI iri_ ->
            NodeIRI iri_

        NodeBlankNode bnode_ ->
            NodeBlankNode bnode_

        NodeLiteral literal_ ->
            NodeLiteral literal_



-- Triple


{-| Triple are the basic building blocks of an RDF Graph. They consiste of two
nodes (subject and object) that are connected by an edge labeled with predicate.
-}
type alias Triple =
    { subject : Node CSubject
    , predicate : Node CPredicate
    , object : Node CObject
    }


{-| Create a new triple
-}
triple : Subject -> Predicate -> Object -> Triple
triple s p o =
    { subject = s, predicate = p, object = o }



-- Namespaces


{-| Helper to create IRIs for a namespace
-}
namespace : String -> String -> IRI
namespace prefix name =
    prefix
        ++ name
        |> iri


{-| The RDF syntax namespace
-}
rdf : String -> IRI
rdf =
    namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#"


{-| The rdf:type predicate
-}
type_ : Predicate
type_ =
    rdf "type" |> predicateIRI


{-| RDF schema namespace
-}
rdfs : String -> IRI
rdfs =
    namespace "http://www.w3.org/2000/01/rdf-schema#"


{-| OWL namespace
-}
owl : String -> IRI
owl =
    namespace "http://www.w3.org/2002/07/owl#"


{-| XSD namespace
-}
xsd : String -> IRI
xsd =
    namespace "http://www.w3.org/2001/XMLSchema#"



-- Graph operations


{-| Graph type

TODO Currently this is implemented as a list of triples. This is not very efficient and there are bette ways of doing this by creating indices for efficient lookup and querying.

-}
type alias Graph =
    List Triple


{-| Description is a graph with a pointer to a subject. This is useful when dealing with certain things in the graph, by providing a starting point for queries.
-}
type alias Description =
    { subject : Subject
    , graph : Graph
    }


graphFilterMap : (Triple -> Maybe a) -> Graph -> List a
graphFilterMap filter graph =
    graph
        |> List.filterMap filter


graphGetObjects : Graph -> Subject -> Predicate -> List Object
graphGetObjects graph subject predicate =
    graphFilterMap
        (\triple_ ->
            if subject == triple_.subject && predicate == triple_.predicate then
                Just triple_.object

            else
                Nothing
        )
        graph


filterForDescriptions : (Triple -> Bool) -> Graph -> List Description
filterForDescriptions filter graph =
    graph
        |> List.filterMap
            (\triple_ ->
                if filter triple_ then
                    { subject = .subject triple_
                    , graph = graph
                    }
                        |> Just

                else
                    Nothing
            )


descriptionGet : Predicate -> Description -> List Object
descriptionGet p description =
    .graph description
        |> List.filterMap
            (\{ subject, predicate, object } ->
                if subject == .subject description && p == predicate then
                    Just object

                else
                    Nothing
            )



-- RDF Decoder
-- This needs to be part of this module in order to be able to access internal representation of the graph.


type alias State =
    { graph : Graph
    , node : Node Never
    }


type alias Error =
    String


{-| A RDF decoder.
-}
type Decoder a
    = Decoder (State -> Result Error ( State, a ))


{-| Run the decoder.
-}
decode : Decoder a -> Graph -> Subject -> Result Error a
decode (Decoder f) graph node =
    f (State graph (rewrapNode node))
        |> Result.map Tuple.second


{-| Run decoder on all subjects in Graph and return all matches.
-}
decodeAll : Decoder a -> Graph -> List a
decodeAll decoder graph =
    graph
        |> graphFilterMap
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
    Decoder (\state -> Err msg)


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
    map2 (\a b -> a) decoderA decoderB


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
iriDecoder : Decoder IRI
iriDecoder =
    Decoder
        (\state ->
            case state.node of
                NodeIRI iri_ ->
                    Ok ( state, iri_ )

                NodeBlankNode _ ->
                    Err "expecting iri, got BlankNode"

                NodeLiteral _ ->
                    Err "expecting IRI, got Literal"
        )


{-| Decode a Literal
-}
literalDecoder : Decoder Literal
literalDecoder =
    Decoder
        (\state ->
            case state.node of
                NodeIRI _ ->
                    Err "expecting Literal, got IRI"

                NodeBlankNode _ ->
                    Err "expecting Literal, got BlankNode"

                NodeLiteral literal_ ->
                    Ok ( state, literal_ )
        )


{-| Decode objects that are linked to current subject via predicate.
-}
objectsDecoder : Predicate -> Decoder a -> Decoder (List a)
objectsDecoder p (Decoder od) =
    Decoder
        (\state ->
            let
                decodeObjects object =
                    od { state | node = rewrapNode object }
            in
            case state.node of
                NodeIRI iri_ ->
                    graphGetObjects state.graph (subjectIRI iri_) p
                        |> List.map decodeObjects
                        |> List.filterMap Result.toMaybe
                        |> List.map Tuple.second
                        |> Tuple.pair state
                        |> Ok

                NodeBlankNode bnode_ ->
                    graphGetObjects state.graph (subjectBlankNode bnode_) p
                        |> List.map decodeObjects
                        |> List.filterMap Result.toMaybe
                        |> List.map Tuple.second
                        |> Tuple.pair state
                        |> Ok

                NodeLiteral _ ->
                    Err "expecting IRI or BlankNode, got Literal"
        )


ensureType : IRI -> Decoder IRI
ensureType iri_ =
    succeed iri_
