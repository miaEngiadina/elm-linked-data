module RDF.JSON exposing (decoder, encode)

{-| Decoder and encoder for the RDF 1.1 JSON Alternate Serialization (RDF/JSON).

Also known as the less insane way of encoding RDF in JSON.

See <https://www.w3.org/TR/rdf-json/>.

-}

import Json.Decode as JD
import Json.Encode as JE
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
        |> JD.map RDF.fromList



-- Encoder


encodeObject : RDF.Object -> JE.Value
encodeObject object =
    RDF.mapObject object
        (\iri ->
            [ ( "type", "uri" |> JE.string )
            , ( "value", JE.string iri )
            ]
                |> JE.object
        )
        (\bnode ->
            [ ( "type", "bnode" |> JE.string )
            , ( "value"
              , "_:"
                    ++ RDF.blankNodeId bnode
                    |> JE.string
              )
            ]
                |> JE.object
        )
        (\literal ->
            [ Just ( "type", "literal" |> JE.string )
            , Just ( "value", literal.value |> JE.string )
            , Just ( "datatype", literal.datatype |> JE.string )
            , literal.language
                |> Maybe.map (\l -> ( "lang", l |> JE.string ))
            ]
                |> List.filterMap identity
                |> JE.object
        )


encodePredicate : RDF.Predicate -> String
encodePredicate p =
    RDF.mapPredicate p identity


encodeSubject : RDF.Subject -> String
encodeSubject s =
    RDF.mapSubject s
        (\iri -> iri)
        RDF.blankNodeId


encode : RDF.Graph -> JE.Value
encode graph =
    graph
        |> RDF.subjects
        |> JE.list
            (\s ->
                [ ( encodeSubject s
                  , RDF.subjectPredicates graph s
                        |> JE.list
                            (\p ->
                                [ ( encodePredicate p
                                  , RDF.graphGetObjects graph s p
                                        |> JE.list encodeObject
                                  )
                                ]
                                    |> JE.object
                            )
                  )
                ]
                    |> JE.object
            )
