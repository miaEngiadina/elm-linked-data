module RDF exposing
    ( BlankNode
    , Description
    , Graph
    , IRI
    , Literal
    , Object
    , Predicate
    , Subject
    , Triple
    , asIRI
    , asLiteral
    , blankNode
    , descriptionGet
    , filterForDescriptions
    , iri
    , literal
    , namespace
    , objectBlankNode
    , objectIRI
    , objectLiteral
    , owl
    , predicateIRI
    , rdf
    , rdfs
    , subjectBlankNode
    , subjectIRI
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
