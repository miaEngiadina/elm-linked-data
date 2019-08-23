module Json.LD exposing (decoder, expand)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.LD.Context as Context exposing (Context)
import Json.Value exposing (JsonValue(..))
import List.Extra
import Maybe
import RDF
import RDF.IRI exposing (IRI)


{-| Decode a JSON-LD document.

You may specify a context that will be used.

-}
decoder : Context -> Decoder RDF.Graph
decoder context =
    Debug.todo "yup, still a long way"


expand : Context -> JsonValue -> JsonValue
expand context input =
    expansionAlgorithm context Nothing input


expansionAlgorithm : Context -> Maybe String -> JsonValue -> JsonValue
expansionAlgorithm context activeProperty element =
    element
