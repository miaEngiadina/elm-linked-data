module Json.LD exposing (decoder, expand)

import AssocList as AL
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
expansionAlgorithm context activeProperty element_ =
    let
        return =
            S.done

        continue =
            S.continue

        isElementScalar element =
            case element of
                StringValue _ ->
                    True

                NumericValue _ ->
                    True

                BoolValue _ ->
                    True

                _ ->
                    False
    in
    element_
        |> S.pure
        -- 1) If element is null, return null.
        |> S.andThen
            (\element ->
                case element of
                    NullValue ->
                        return NullValue

                    _ ->
                        continue element
            )
        -- 2) If element is a scalar,
        |> S.ifAndThen
            isElementScalar
            (\element ->
                case activeProperty of
                    Nothing ->
                        return NullValue

                    Just "@graph" ->
                        return NullValue

                    Just property ->
                        case valueExpansion context property element of
                            Ok expandedValue ->
                                return expandedValue

                            Err error ->
                                error |> S.fail
            )
        -- 3) If element is an array,
        |> S.maybeAndThen
            (\element ->
                case element of
                    ArrayValue items ->
                        Just items

                    _ ->
                        Nothing
            )
            (\items state_ ->
                List.foldl
                    -- 3.2) For each item in element:
                    (\item result ->
                        -- 3.2.1) Initialize expanded item to the result of using this algorithm recursively, passing active context, active property, and item as element.
                        let
                            expandedItemResult =
                                expansionAlgorithm context activeProperty item
                        in
                        case expandedItemResult of
                            Err error ->
                                error |> S.fail

                            Ok expandedItem ->
                                let
                                    isExpandedItemAnArrayOrAListObject =
                                        case expandedItem of
                                            ArrayValue _ ->
                                                True

                                            ObjectValue values ->
                                                values
                                                    |> AL.member "@list"

                                            _ ->
                                                False

                                    activePropertyIsList =
                                        activeProperty
                                            |> Maybe.map ((==) "@list")
                                            |> Maybe.withDefault False

                                    activePropertyContainerMappingIsSetToList =
                                        activeProperty
                                            |> Maybe.andThen (Context.getContainerMapping context)
                                            |> Maybe.map ((==) "@list")
                                            |> Maybe.withDefault False
                                in
                                -- 3.2.2) If the active property is @list or its container mapping is set to @list, the expanded item must not be an array or a list object, otherwise a list of lists error has been detected and processing is aborted.
                                if (activePropertyIsList || activePropertyContainerMappingIsSetToList) && isExpandedItemAnArrayOrAListObject then
                                    ListOfLists |> S.fail

                                else
                                    -- 3.2.3) If expanded item is an array, append each of its items to result. Otherwise, if expanded item is not null, append it to result.
                                    case expandedItem of
                                        ArrayValue listOfExpandedItems ->
                                            result
                                                |> S.map (List.append listOfExpandedItems)

                                        NullValue ->
                                            result

                                        _ ->
                                            result
                                                |> S.map ((::) expandedItem)
                    )
                    -- 3.1) Initialize an empty array, result.
                    ([] |> S.pure)
                    items
                    |> S.map ArrayValue
                    -- 3.3) Return result
                    |> S.andThen return
            )
        |> S.andThen
            (\element ->
                case element of
                    -- 4) Otherwise element is a JSON object.
                    ObjectValue elementValues_ ->
                        { elementValues = elementValues_
                        , context = context

                        -- 6) Initialize an empty JSON object, result.
                        , result = []
                        }
                            |> S.pure
                            -- 5) If element contains the key @context, set active context to the result of the Context Processing algorithm, passing active context and the value of the @context key as local context.
                            |> S.maybeAndThen
                                (\{ elementValues } -> AL.get "@context" elementValues)
                                (\localContext state ->
                                    case Context.update [] localContext state.context of
                                        Ok updatedContext ->
                                            { state | context = updatedContext }
                                                |> continue

                                        Err ( _, error ) ->
                                            error |> S.fail
                                )
                            -- 7) For each key and value in element, ordered lexicographically by key:
                            |> S.andThen
                                (\state ->
                                    List.foldl
                                        (\( key, value ) s ->
                                            if key == "@context" then
                                                -- 7.1) If key is @context, continue to the next key.
                                                s

                                            else
                                                -- 7.2) Set expanded property to the result of using the IRI Expansion algorithm, passing active context, key for value, and true for vocab.
                                                case Context.expandIRI state.context False True Nothing Nothing key of
                                                    Ok expandedProperty ->
                                                        if String.isEmpty expandedProperty || (not (String.contains ":" expandedProperty) && not (List.member expandedProperty Context.keywords)) then
                                                            -- 7.3) If expanded property is null or it neither contains a colon (:) nor it is a keyword, drop key by continuing to the next key.
                                                            s

                                                        else if List.member expandedProperty Context.keywords then
                                                            -- 7.4) If expanded property is a keyword:
                                                            -- TODO
                                                            s

                                                        else
                                                            s

                                                    Err ( _, e ) ->
                                                        e |> S.fail
                                        )
                                        (state |> S.pure)
                                        state.elementValues
                                )
                            |> S.map (\{ result } -> ObjectValue result)

                    _ ->
                        -- If element is not a JSON object
                        AlgorithmDidNotReturn |> S.fail
            )
        |> S.andThen return
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
