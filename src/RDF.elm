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


{-| Construct an IRI
-}
iri : String -> IRI
iri =
    identity


{-| A blank node
-}
type BlankNode
    = BNode


{-| Create a new blank node.
-}
blankNode : () -> BlankNode
blankNode () =
    BNode


type alias Literal =
    ()



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
    = IRI IRI
    | BlankNode BlankNode
    | Literal Literal



-- Subject


{-| Subject which is an IRI or a blank node
-}
type alias Subject =
    Node CSubject


{-| Create a subject from an IRI
-}
subjectIRI : IRI -> Subject
subjectIRI i =
    IRI i


{-| Create a subject from a blank node
-}
subjectBlankNode : BlankNode -> Subject
subjectBlankNode bnode =
    BlankNode bnode



-- Predicate


{-| Predicate which is an IRI
-}
type alias Predicate =
    Node CPredicate


{-| Create a predicate from an IRI
-}
predicateIRI : IRI -> Predicate
predicateIRI i =
    IRI i



-- Object


{-| Objects which can be a IRI, a blank node or a literal
-}
type alias Object =
    Node CObject


objectIRI : IRI -> Object
objectIRI i =
    IRI i


objectBlankNode : BlankNode -> Object
objectBlankNode bnode =
    BlankNode bnode


objectLiteral : Literal -> Object
objectLiteral l =
    Literal l



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
