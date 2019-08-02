module Json.LD.Context exposing (Context, decoder, update)

{-| JSON-LD Context

This module contains the context related algorithms described in "JSON-LD
1.0 Processing Algorithms and API" (<https://www.w3.org/TR/json-ld-api/>)

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Value exposing (JsonValue(..))
import List.Extra
import RDF.IRI exposing (IRI)



-- Association List helpers


type alias AssocList a =
    List ( String, a )


{-| Get element with specified key from an association list
-}
get : String -> AssocList a -> Maybe a
get key assocList =
    List.Extra.find (\( k, v ) -> k == key) assocList
        |> Maybe.map Tuple.second



--


{-| JSON-LD Context (see: <https://www.w3.org/TR/json-ld/#the-context>)
-}
type alias Context =
    { -- The active context contains the active term definitions which specify how properties and values have to be interpreted
      termDefinitions : Dict String TermDefinition

    -- as well as the current base IRI
    , baseIRI : Maybe IRI

    -- the vocabulary mapping
    , vocabularyMapping : Maybe IRI

    -- the default language.
    , defaultLanguage : Maybe String
    }


{-| An empty context
-}
empty : Context
empty =
    { termDefinitions = Dict.empty
    , baseIRI = Nothing
    , vocabularyMapping = Nothing
    , defaultLanguage = Nothing
    }


{-| TODO
-}
type alias TermDefinition =
    -- Each term definition consists of
    { -- an IRI mapping,
      mapping : String

    -- a boolean flag reverse property,
    , reverse : Bool

    --an optional type mapping or language mapping,
    -- TODO
    -- and an optional container mapping.
    -- TODO
    }



-- Error handling


type Error
    = InvalidLocalContext
    | InvalidBaseIRI
    | InvalidVocabMapping
    | InvalidDefaultLanguage


{-| Json.Decode requires errors to be printable as string
-}
errorToString : Error -> String
errorToString error =
    case error of
        InvalidLocalContext ->
            "invalid local context"

        InvalidBaseIRI ->
            "invalid base IRI"

        InvalidVocabMapping ->
            "invalid vocab mapping"

        InvalidDefaultLanguage ->
            "invalid default language"


{-| Helper to convert Result to Json.Decode.Decoder
-}
resultToDecoder : Result Error Context -> Decoder Context
resultToDecoder result =
    case result of
        Ok c ->
            JD.succeed c

        Err e ->
            e
                |> errorToString
                |> JD.fail



-- Context Processing Algorithms


{-| Update active context with a local context (see <https://www.w3.org/TR/json-ld-api/#context-processing-algorithms>)
-}
update : List IRI -> JsonValue -> Context -> Result Error Context
update remote local active =
    case local of
        NullValue ->
            -- 3.1) If context is null, set result to a newly-initialized active context and continue with the next context. The base IRI of the active context is set to the IRI of the currently being processed document (which might be different from the currently being processed context), if available; otherwise to null. If set, the base option of a JSON-LD API Implementation overrides the base IRI.
            Ok empty

        StringValue value ->
            -- 3.2) If context is a string,
            Debug.todo "resolve remote context"

        ObjectValue contextValues ->
            active
                |> updateBaseIRI remote contextValues
                |> Result.andThen (updateVocabMapping contextValues)
                |> Result.andThen (updateDefaultLanguage contextValues)

        _ ->
            -- 3.3) If context is not a JSON object, an invalid local context error has been detected and processing is aborted.
            Err InvalidLocalContext



-- update helpers


updateBaseIRI : List String -> List ( String, JsonValue ) -> Context -> Result Error Context
updateBaseIRI remoteContexts contextValues result =
    -- 3.4) If context has an @base key and remote contexts is empty, i.e., the currently being processed context is not a remote context:
    case remoteContexts of
        [] ->
            case get "@base" contextValues of
                Just NullValue ->
                    -- 3.4.2) If value is null, remove the base IRI of result.
                    Ok { result | baseIRI = Nothing }

                Just (StringValue value) ->
                    -- 3.4.3) Otherwise, if value is an absolute IRI, the base IRI of result is set to value.
                    -- 3.4.4) Otherwise, if value is a relative IRI and the base IRI of result is not null, set the base IRI of result to the result of resolving value against the current base IRI of result.
                    Debug.todo "decide if absolute or relative IRI"

                Just _ ->
                    -- 3.4.5) Otherwise, an invalid base IRI error has been detected and processing is aborted.
                    Err InvalidBaseIRI

                Nothing ->
                    Ok result

        _ ->
            Ok result


updateVocabMapping : List ( String, JsonValue ) -> Context -> Result Error Context
updateVocabMapping contextValues result =
    -- 3.5) If context has an @vocab key:
    case get "@vocab" contextValues of
        Just NullValue ->
            -- 3.5.2) If value is null, remove any vocabulary mapping from result.
            Ok { result | vocabularyMapping = Nothing }

        Just (StringValue value) ->
            -- 3.5.3) Otherwise, if value is an absolute IRI or blank node identifier, the vocabulary mapping of result is set to value. If it is not an absolute IRI or blank node identifier, an invalid vocab mapping error has been detected and processing is aborted.
            Debug.todo "handle IRI"

        Just _ ->
            Err InvalidVocabMapping

        Nothing ->
            Ok result


updateDefaultLanguage : List ( String, JsonValue ) -> Context -> Result Error Context
updateDefaultLanguage contextValues result =
    case get "@language" contextValues of
        -- 3.6) If context has an @language key:
        Just NullValue ->
            -- 3.6.2) If value is null, remove any default language from result.
            Ok { result | defaultLanguage = Nothing }

        Just (StringValue value) ->
            -- 3.6.3) Otherwise, if value is string, the default language of result is set to lowercased value.
            Ok { result | defaultLanguage = value |> String.toLower |> Just }

        Just _ ->
            -- If it is not a string, an invalid default language error has been detected and processing is aborted.
            Err InvalidDefaultLanguage

        Nothing ->
            Ok result



--


{-| Decode a JSON-LD context
-}
decoder : Decoder Context
decoder =
    Json.Value.decoder
        |> JD.andThen
            (\json ->
                case json of
                    ObjectValue values ->
                        case get "@context" values of
                            Nothing ->
                                JD.succeed empty

                            Just local ->
                                update [] local empty
                                    |> resultToDecoder

                    _ ->
                        JD.succeed empty
            )
