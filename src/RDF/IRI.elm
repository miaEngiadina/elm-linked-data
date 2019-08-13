module RDF.IRI exposing
    ( IRI
    , fromString
    , isAbsolute
    , isRelative
    , split
    )

{-| Internationalized Resource Identifiers (IRIs)

See <https://www.ietf.org/rfc/rfc3987.txt>.

But hey what is an IRI?

      It's like an URI, but with Unicode support.

URI?

    A Uniform Resource Identifier (https://tools.ietf.org/rfc/rfc3986.txt).

Is that a typo? Shouldn't that be URL?

    Nope. It's URI. URL is a kind of URI. Yup, it's confusing. Read about it here: https://danielmiessler.com/study/url-uri/.

What about the elm/url library?

    Really it deals with IRIs as it supports Unicode. It's called elm/url because mostly it is used with HTTP that requires locations (URLs).

So then what is this module?

    I don't know yet. elm/url does not yet do all of what I need to do with IRIs. I'm still figuring out what I need...

-}


{-| TODO: The typed nicer way to do this would be something like: <https://github.com/nikita-volkov/iri/blob/master/library/Iri/Data/Types.hs#L85>
-}
type alias IRI =
    String


fromString : String -> IRI
fromString =
    identity


{-| TODO
-}
isRelative : IRI -> Bool
isRelative iri =
    False


{-| TODO
-}
isAbsolute : IRI -> Bool
isAbsolute iri =
    True


split : IRI -> Maybe ( String, String )
split iri =
    case String.split ":" iri of
        [] ->
            Nothing

        nocolon :: [] ->
            Nothing

        prefix :: suffixList ->
            Just ( prefix, String.concat suffixList )
