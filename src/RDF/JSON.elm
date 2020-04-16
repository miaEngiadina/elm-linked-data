module RDF.JSON exposing (decoder)

{-| Decoder and encoder for the RDF 1.1 JSON Alternate Serialization (RDF/JSON).

Also known as the less insane way of encoding RDF in JSON.

See <https://www.w3.org/TR/rdf-json/>.

-}

import Json.Decode as JD
import RDF



-- Decoders for ValueObject


valueObjectDecoder : JD.Decoder RDF.Object
valueObjectDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "uri" ->
                        JD.field "value" JD.string
                            |> JD.map (RDF.iri >> RDF.objectIRI)

                    "bnode" ->
                        JD.field "value" JD.string
                            |> JD.map (String.dropLeft 2 >> RDF.blankNode >> RDF.objectBlankNode)

                    "literal" ->
                        JD.map3 RDF.literal
                            (JD.field "value" JD.string)
                            (JD.maybe
                                (JD.field "datatype" JD.string
                                    |> JD.map RDF.iri
                                )
                                |> JD.map (Maybe.withDefault (RDF.xsd "string"))
                            )
                            (JD.maybe (JD.field "lang" JD.string))
                            |> JD.map RDF.objectLiteral

                    _ ->
                        JD.fail <| "Illegal type in value object (" ++ type_ ++ ")"
            )



-- Decoder for SubjectObject


type alias SubjectObject =
    List ( RDF.Predicate, RDF.Object )


predicateDecoder : String -> RDF.Predicate
predicateDecoder =
    RDF.iri >> RDF.predicateIRI


subjectObjectDecoder : JD.Decoder SubjectObject
subjectObjectDecoder =
    JD.keyValuePairs (JD.list valueObjectDecoder)
        |> JD.map
            (List.concatMap
                (\( predicateKey, objects ) ->
                    let
                        predicate =
                            predicateDecoder predicateKey
                    in
                    List.map (\object -> ( predicate, object )) objects
                )
            )



-- Other helpers


{-| Decode the key that denotes the subject to a RDF.Subject
-}
subjectDecoder : String -> RDF.Subject
subjectDecoder key =
    if String.startsWith "_:" key then
        String.dropLeft 2 key
            |> RDF.blankNode
            |> RDF.subjectBlankNode

    else
        key
            |> RDF.iri
            |> RDF.subjectIRI


{-| Returns a list of triples for a given subject and a SubjectObject
-}
subjectObjectToTriples : RDF.Subject -> SubjectObject -> List RDF.Triple
subjectObjectToTriples subject subjectObject =
    List.map
        (\( predicate, object ) ->
            RDF.triple subject predicate object
        )
        subjectObject


{-| Decode a JSON as RDF/JSON to a RDF.Graph
-}
decoder : JD.Decoder RDF.Graph
decoder =
    JD.keyValuePairs subjectObjectDecoder
        |> JD.map
            (List.concatMap
                (\( subjectKey, subjectObject ) ->
                    subjectObjectToTriples
                        (subjectDecoder subjectKey)
                        subjectObject
                )
            )
