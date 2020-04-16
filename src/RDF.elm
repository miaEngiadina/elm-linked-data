module RDF exposing
    ( BlankNode
    , Graph
    , IRI
    , Literal
    , Object
    , Predicate
    , Subject
    , Triple
    , blankNode
    , iri
    , literal
    , objectBlankNode
    , objectIRI
    , objectLiteral
    , predicateIRI
    , subjectBlankNode
    , subjectIRI
    , triple
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


type alias Triple =
    { subject : Node CSubject
    , predicate : Node CPredicate
    , object : Node CObject
    }


triple : Subject -> Predicate -> Object -> Triple
triple s p o =
    { subject = s, predicate = p, object = o }


type alias Graph =
    List Triple
