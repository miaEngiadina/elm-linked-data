module Json.LD exposing (decoder, expand)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.LD.Context as Context exposing (Context)
import Json.LD.Error as Error exposing (Error(..))
import Json.Value exposing (JsonValue(..))
import List.Extra
import Maybe
import RDF
import RDF.IRI exposing (IRI)
import Spaghetti as S


{-| Decode a JSON-LD document.

You may specify a context that will be used.

-}
decoder : Context -> Decoder RDF.Graph
decoder context =
    Debug.todo "yup, still a long way"


expand : Context -> JsonValue -> Result Error JsonValue
expand context input =
    expansionAlgorithm context Nothing input


expansionAlgorithm : Context -> Maybe String -> JsonValue -> Result Error JsonValue
expansionAlgorithm context activeProperty_ element_ =
    let
        return =
            S.done

        continue =
            S.continue

        isElementScalar state =
            case state.element of
                StringValue _ ->
                    True

                NumericValue _ ->
                    True

                BoolValue _ ->
                    True

                _ ->
                    False
    in
    { element = element_
    , activeProperty = activeProperty_
    }
        |> S.pure
        -- 1) If element is null, return null.
        |> S.andThen
            (\state ->
                case state.element of
                    NullValue ->
                        return NullValue

                    _ ->
                        continue state
            )
        -- 2) If element is a scalar,
        |> S.ifAndThen
            isElementScalar
            (\state ->
                case state.activeProperty of
                    Nothing ->
                        return NullValue

                    Just "@graph" ->
                        return NullValue

                    Just property ->
                        case valueExpansion context property state.element of
                            Ok expandedValue ->
                                return expandedValue

                            Err e ->
                                e |> S.fail
            )
        -- 3) If element is an array,
        |> S.andThen
            (\state ->
                case state.element of
                    ArrayValue items ->
                        continue state

                    _ ->
                        continue state
            )
        |> S.andThen (\state -> return state.element)
        |> S.toResult (\_ -> Err AlgorithmDidNotReturn)


valueExpansion : Context -> String -> JsonValue -> Result Error JsonValue
valueExpansion context_ property_ value_ =
    let
        return =
            S.done

        continue =
            S.continue

        addToResult key value state =
            { state | result = ( key, value ) :: state.result }
    in
    { context = context_
    , property = property_
    , value = value_
    }
        |> S.pure
        -- 1) If the active property has a type mapping in active context that is @id, return a new JSON object containing a single key-value pair where the key is @id and the value is the result of using the IRI Expansion algorithm, passing active context, value, and true for document relative.
        |> S.ifAndThen
            (\{ context, property } ->
                case Context.getTypeMapping context property of
                    Just "@id" ->
                        True

                    _ ->
                        False
            )
            (\state ->
                case state.value of
                    StringValue value ->
                        case Context.expandIRI state.context True False Nothing Nothing value of
                            Ok expandedValue ->
                                [ ( "@id", StringValue expandedValue ) ] |> ObjectValue |> return

                            Err ( _, error ) ->
                                error |> S.fail

                    _ ->
                        -- Don't know what to do if value is not a StringValue
                        state |> S.continue
            )
        -- 2) If active property has a type mapping in active context that is @vocab, return a new JSON object containing a single key-value pair where the key is @id and the value is the result of using the IRI Expansion algorithm, passing active context, value, true for vocab, and true for document relative.
        |> S.ifAndThen
            (\{ context, property } ->
                case Context.getTypeMapping context property of
                    Just "@vocab" ->
                        True

                    _ ->
                        False
            )
            (\state ->
                case state.value of
                    StringValue value ->
                        case Context.expandIRI state.context True True Nothing Nothing value of
                            Ok expandedValue ->
                                [ ( "@id", StringValue expandedValue ) ] |> ObjectValue |> return

                            Err ( _, error ) ->
                                error |> S.fail

                    _ ->
                        -- Don't know what to do if value is not a StringValue
                        state |> S.continue
            )
        -- 3) Otherwise, initialize result to a JSON object with an @value member whose value is set to value.
        |> S.andThen
            (\state ->
                { context = state.context
                , property = state.property
                , value = state.value
                , result = [ ( "@value", state.value ) ]
                }
                    |> continue
            )
        -- 4) If active property has a type mapping in active context, add an @type member to result and set its value to the value associated with the type mapping.
        |> S.maybeAndThen
            (\{ context, property } ->
                Context.getTypeMapping context property
            )
            (\typeMapping state ->
                state
                    |> addToResult "@type" (StringValue typeMapping)
                    |> continue
            )
        -- 5) Otherwise, if value is a string:
        |> S.ifAndThen
            (\{ value } ->
                case value of
                    StringValue _ ->
                        True

                    _ ->
                        False
            )
            (\state ->
                case Context.getLanguageMapping state.context state.property of
                    -- If a language mapping is associated with active property in active context, add an @language to result and set its value to the language code associated with the language mapping; unless the language mapping is set to null in which case no member is added.
                    Just languageMapping ->
                        state
                            |> addToResult "@language" (StringValue languageMapping)
                            |> continue

                    _ ->
                        case state.context.defaultLanguage of
                            -- Otherwise, if the active context has a default language, add an @language to result and set its value to the default language.
                            Just languageMapping ->
                                state
                                    |> addToResult "@language" (StringValue languageMapping)
                                    |> continue

                            Nothing ->
                                state |> continue
            )
        -- 6) Return result.
        |> S.andThen
            (\state ->
                state.result |> ObjectValue |> return
            )
        |> S.toResult (\_ -> Err AlgorithmDidNotReturn)
