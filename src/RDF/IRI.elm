module RDF.IRI exposing (IRI)

{-| Internationalized Resource Identifiers (IRIs)

See <https://www.ietf.org/rfc/rfc3987.txt>

-}


type alias IRI =
    String


fromString : String -> IRI
fromString =
    identity
