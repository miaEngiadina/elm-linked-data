module Json.LD.Context exposing (Context, decoder, empty, update)

{-| JSON-LD Context

This module contains the context related algorithms described in "JSON-LD
1.0 Processing Algorithms and API" (<https://www.w3.org/TR/json-ld-api/>)

-}

import AssocList as AL exposing (AssocList)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.LD.Error as Error exposing (Error(..))
import Json.Value exposing (JsonValue(..))
import RDF.IRI as IRI exposing (IRI)
import Spaghetti as S


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


type alias TermDefinition =
    -- Each term definition consists of
    { -- an IRI mapping,
      mapping : String

    -- a boolean flag reverse property,
    , reverse : Bool

    -- an optional type mapping
    , typeMapping : Maybe String

    -- or language mapping,
    , languageMapping : Maybe String

    -- and an optional container mapping.
    , containerMapping : Maybe String
    }



-- Spaghetti code handling helpers


{-| The internal return type for algorithms and sub-algorithms
-}
type alias State value result =
    S.State ( List String, Error ) value result



-- Result ( List String, Error ) Context


addErrorMsg : String -> State a result -> State a result
addErrorMsg msg state =
    state
        |> S.mapError (\( stack, error ) -> ( msg :: stack, error ))


{-| Bind operator that adds stack tracing like information for error handling
-}
andThen : String -> (a -> State b result) -> State a result -> State b result
andThen msg f x =
    S.andThen f x
        |> addErrorMsg msg


maybeAndThen : String -> (a -> Maybe b) -> (b -> a -> State a result) -> State a result -> State a result
maybeAndThen msg get f x =
    S.maybeAndThen get f x
        |> addErrorMsg msg


fail : String -> Error -> State value result
fail msg error =
    S.fail ( [ msg ], error )


failWithStack : List String -> String -> Error -> State value result
failWithStack stack msg error =
    S.fail ( msg :: stack, error )


{-| Helper to convert Result to Json.Decode.Decoder
-}
resultToDecoder : Result ( List String, Error ) Context -> Decoder Context
resultToDecoder result =
    case result of
        Ok c ->
            JD.succeed c

        Err ( ctx, e ) ->
            String.concat
                [ "Processing JSON-LD context failed ("
                , ctx |> String.join "."
                , "): "
                , e |> Error.toString
                ]
                |> JD.fail



-- Context Processing Algorithms


{-| Update active context with a local context (see <https://www.w3.org/TR/json-ld-api/#context-processing-algorithms>)
-}
update : List IRI -> JsonValue -> Context -> Result ( List String, Error ) Context
update remote localContextValue active =
    let
        mapResult f x =
            { x | result = f x.result }
    in
    -- 1) Initialize result to the result of cloning active context.
    { result = active }
        |> S.pure
        |> andThen "3"
            (\state_ ->
                case localContextValue of
                    NullValue ->
                        -- 3.1) If context is null, set result to a newly-initialized active context and continue with the next context. The base IRI of the active context is set to the IRI of the currently being processed document (which might be different from the currently being processed context), if available; otherwise to null. If set, the base option of a JSON-LD API Implementation overrides the base IRI.
                        S.done empty

                    StringValue value ->
                        -- 3.2) If context is a string,
                        Debug.todo "resolve remote context"

                    ObjectValue local ->
                        let
                            getFromLocal key x =
                                x.local |> AL.get key
                        in
                        { result = state_.result
                        , local = local
                        }
                            |> S.pure
                            |> maybeAndThen "4"
                                (getFromLocal "@base")
                                (\baseValue state ->
                                    if List.isEmpty remote then
                                        -- 3.4) If context has an @base key and remote contexts is empty, i.e., the currently being processed context is not a remote context:
                                        case baseValue of
                                            NullValue ->
                                                -- 3.4.2) If value is null, remove the base IRI of result.
                                                state
                                                    |> mapResult (\result -> { result | baseIRI = Nothing })
                                                    |> S.continue

                                            StringValue value ->
                                                let
                                                    iri =
                                                        IRI.fromString value
                                                in
                                                if IRI.isAbsolute iri then
                                                    -- 3.4.3) Otherwise, if value is an absolute IRI, the base IRI of result is set to value.
                                                    state
                                                        |> mapResult (\result -> { result | baseIRI = Just iri })
                                                        |> S.continue

                                                else if IRI.isRelative iri then
                                                    -- 3.4.4) Otherwise, if value is a relative IRI and the base IRI of result is not null, set the base IRI of result to the result of resolving value against the current base IRI of result.
                                                    -- TODO: "resolve against the current base IRI"
                                                    state
                                                        |> mapResult
                                                            (\result -> { result | baseIRI = Just iri })
                                                        |> S.continue

                                                else
                                                    -- 3.4.5) Otherwise, an invalid base IRI error has been detected and processing is aborted.
                                                    fail "5" InvalidBaseIRI

                                            _ ->
                                                -- 3.4.5) Otherwise, an invalid base IRI error has been detected and processing is aborted.
                                                fail "5" InvalidBaseIRI

                                    else
                                        state |> S.continue
                                )
                            |> maybeAndThen "5"
                                (getFromLocal "@vocab")
                                (\vocabValue state ->
                                    case vocabValue of
                                        NullValue ->
                                            -- 3.5.2) If value is null, remove any vocabulary mapping from result.
                                            state
                                                |> mapResult (\result -> { result | vocabularyMapping = Nothing })
                                                |> S.continue

                                        StringValue value ->
                                            -- 3.5.3) Otherwise, if value is an absolute IRI or blank node identifier, the vocabulary mapping of result is set to value. If it is not an absolute IRI or blank node identifier, an invalid vocab mapping error has been detected and processing is aborted.
                                            if IRI.fromString value |> IRI.isAbsolute then
                                                state
                                                    |> mapResult (\result -> { result | vocabularyMapping = Just value })
                                                    |> S.continue

                                            else if isBlankNodeIdentifier value then
                                                state
                                                    |> mapResult (\result -> { result | vocabularyMapping = Just value })
                                                    |> S.continue

                                            else
                                                fail "5" InvalidVocabMapping

                                        _ ->
                                            fail "5" InvalidVocabMapping
                                )
                            |> maybeAndThen "6"
                                (getFromLocal "@language")
                                (\languageValue state ->
                                    case languageValue of
                                        -- 3.6) If context has an @language key:
                                        NullValue ->
                                            -- 3.6.2) If value is null, remove any default language from result.
                                            state
                                                |> mapResult (\result -> { result | defaultLanguage = Nothing })
                                                |> S.continue

                                        StringValue value ->
                                            -- 3.6.3) Otherwise, if value is string, the default language of result is set to lowercased value.
                                            state
                                                |> mapResult
                                                    (\result ->
                                                        { result | defaultLanguage = value |> String.toLower |> Just }
                                                    )
                                                |> S.continue

                                        _ ->
                                            -- If it is not a string, an invalid default language error has been detected and processing is aborted.
                                            fail "" InvalidDefaultLanguage
                                )
                            |> andThen "7"
                                -- 3.7) Create a JSON object defined to use to keep track of whether or not a term has already been defined or currently being defined during recursion.
                                (\state ->
                                    { result = state.result
                                    , local = state.local
                                    , defined = Dict.empty
                                    }
                                        |> S.pure
                                )
                            |> andThen "8"
                                (\state__ ->
                                    List.foldl
                                        (\term ->
                                            S.andThen
                                                (\state ->
                                                    case createTermDefinition state.local term state.result state.defined of
                                                        Ok ( result, defined ) ->
                                                            { state | result = result, defined = defined }
                                                                |> S.continue

                                                        Err ( stack, error ) ->
                                                            failWithStack stack ("createTermDefinition(" ++ term ++ ")") error
                                                )
                                        )
                                        (S.pure state__)
                                        (state__.local
                                            |> AL.remove "@base"
                                            |> AL.remove "@vocab"
                                            |> AL.remove "@language"
                                            |> AL.keys
                                        )
                                )

                    _ ->
                        -- 3.3) If context is not a JSON object, an invalid local context error has been detected and processing is aborted.
                        fail "3" InvalidLocalContext
            )
        |> S.toResult (\_ -> Err ( [], InvalidLocalContext ))


{-| 6.2 Create Term Definition

This algorithm is called from the Context Processing algorithm to create a term definition in the active context for a term being processed in a local context.

-}
createTermDefinition : AssocList JsonValue -> String -> Context -> Dict String Bool -> Result ( List String, Error ) ( Context, Dict String Bool )
createTermDefinition local term active defined =
    let
        -- Helpers to map state fields
        mapActive f x =
            { x | active = f x.active }

        setDefined d x =
            { x | defined = x.defined |> Dict.insert term d }

        mapDefinition f x =
            { x | definition = f x.definition }

        setDefinitionInActiveContext x =
            x
                |> mapActive
                    (\context ->
                        { context
                            | termDefinitions = context.termDefinitions |> Dict.insert term x.definition
                        }
                    )

        return x =
            ( x.active, x.defined )
                |> S.done

        getFromValue key x =
            AL.get key x.value
    in
    { active = active
    , defined = defined
    }
        -- initialize state
        |> S.continue
        |> S.andThen
            (\state ->
                case Dict.get term defined of
                    Just True ->
                        -- 1) if defined contains the key term and the associated value is true (indicating that the term definition has already been created), return.
                        state
                            |> return

                    Just False ->
                        -- Otherwise, if the value is false, a cyclic IRI mapping error has been detected and processing is aborted.
                        fail "1" CyclicIRIMapping

                    Nothing ->
                        state
                            |> S.continue
            )
        -- 2) Set the value associated with defined's term key to false. This indicates that the term definition is now being created but is not yet complete.
        |> S.map (setDefined False)
        -- 3) Since keywords cannot be overridden, term must not be a keyword. Otherwise, a keyword redefinition error has been detected and processing is aborted.
        |> S.andThen
            (\state ->
                if List.member term keywords then
                    fail "3" KeywordRedefinition

                else
                    state
                        |> S.continue
            )
        -- 4) Remove any existing term definition for term in active context.
        |> S.map
            (mapActive
                (\context ->
                    { context | termDefinitions = context.termDefinitions |> Dict.remove term }
                )
            )
        -- 5) Initialize value to a copy of the value associated with the key term in local context.
        |> S.map
            (\state ->
                { active = state.active
                , defined = state.defined
                , value =
                    AL.get term local
                        --  We always get a value back as we are iterating over keys of local.
                        |> Maybe.withDefault NullValue
                }
            )
        -- 6) If value is null or value is a JSON object containing the key-value pair @id-null, set the term definition in active context to null, set the value associated with defined's key term to true, and return.
        |> S.andThen
            (\state ->
                case state.value of
                    NullValue ->
                        state
                            |> mapActive (\context -> { context | termDefinitions = context.termDefinitions |> Dict.remove term })
                            |> setDefined True
                            |> return

                    ObjectValue v ->
                        case AL.get "@id" v of
                            Just NullValue ->
                                state
                                    |> mapActive (\context -> { context | termDefinitions = context.termDefinitions |> Dict.remove term })
                                    |> setDefined True
                                    |> return

                            _ ->
                                state |> S.continue

                    _ ->
                        state |> S.continue
            )
        |> S.andThen
            (\state ->
                case state.value of
                    StringValue id ->
                        -- 7) Otherwise, if value is a string, convert it to a JSON object consisting of a single member whose key is @id and whose value is value.
                        { active = state.active
                        , defined = state.defined
                        , value = [ ( "@id", StringValue id ) ]
                        }
                            |> S.continue

                    ObjectValue v ->
                        -- 8) Otherwise, value must be a JSON object,
                        { active = state.active
                        , defined = state.defined
                        , value = v
                        }
                            |> S.continue

                    _ ->
                        -- Otherwise, value must be a JSON object, if not, an invalid term definition error has been detected and processing is aborted.
                        fail "8" InvalidTermDefinition
            )
        -- 9) Create a new term definition, definition.
        |> S.map
            (\state ->
                { active = state.active
                , defined = state.defined
                , value = state.value
                , definition =
                    { mapping = ""
                    , reverse = False
                    , typeMapping = Nothing
                    , languageMapping = Nothing
                    , containerMapping = Nothing
                    }
                }
            )
        -- 10) If value contains the key @type:
        |> S.maybeAndThen (getFromValue "@type")
            (\typeValue state_ ->
                case typeValue of
                    StringValue type__ ->
                        -- 10.1) Initialize type to the value associated with the @type key, which must be a string.
                        ( type__, state_ )
                            |> S.continue
                            -- 10.2) Set type to the result of using the IRI Expansion algorithm, passing active context, type for value, true for vocab, false for document relative, local context, and defined. If the expanded type is neither @id, nor @vocab, nor an absolute IRI, an invalid type mapping error has been detected and processing is aborted.
                            |> S.andThen
                                (\( type_, state ) ->
                                    case expandIRI state.active False True (Just local) (Just state.defined) type_ of
                                        Ok expandedTypeIRI ->
                                            ( expandedTypeIRI, state ) |> S.continue

                                        Err e ->
                                            e |> S.fail
                                )
                            -- 10.3) Set the type mapping for definition to type.
                            |> S.map
                                (\( type_, state ) ->
                                    state
                                        |> mapDefinition (\definition -> { definition | typeMapping = Just type_ })
                                )

                    _ ->
                        -- Otherwise, an invalid type mapping error has been detected and processing is aborted.
                        fail "10" InvalidTypeMapping
            )
        -- 11) If value contains the key @reverse:
        |> S.maybeAndThen (getFromValue "@reverse")
            (\reverseValue state_ ->
                state_
                    |> S.continue
                    -- 11.1) If value contains an @id, member, an invalid reverse property error has been detected and processing is aborted.
                    |> S.maybeAndThen (getFromValue "@id") (\_ _ -> InvalidReverseProperty |> fail "11.1")
                    -- 11.2) If the value associated with the @reverse key is not a string, an invalid IRI mapping error has been detected and processing is aborted.
                    |> S.andThen
                        (\state ->
                            case reverseValue of
                                StringValue _ ->
                                    state |> S.continue

                                _ ->
                                    fail "11.2" InvalidIRIMapping
                        )
                    -- 11.3) Otherwise, set the IRI mapping of definition to the result of using the IRI Expansion algorithm, passing active context, the value associated with the @reverse key for value, true for vocab, false for document relative, local context, and defined. If the result is neither an absolute IRI nor a blank node identifier, i.e., it contains no colon (:), an invalid IRI mapping error has been detected and processing is aborted.
                    |> S.andThen
                        (\state ->
                            case reverseValue of
                                StringValue reverse ->
                                    case expandIRI state.active False True (Just local) (Just state.defined) reverse of
                                        Ok expandedReverseMapping ->
                                            if not (String.contains ":" expandedReverseMapping) then
                                                InvalidIRIMapping |> fail "11.3"

                                            else
                                                state
                                                    |> mapDefinition (\definition -> { definition | mapping = expandedReverseMapping })
                                                    |> S.continue

                                        Err e ->
                                            e |> S.fail

                                _ ->
                                    InvalidIRIMapping |> fail "11"
                        )
                    -- 11.4) If value contains an @container member, set the container mapping of definition to its value; if its value is neither @set, nor @index, nor null, an invalid reverse property error has been detected (reverse properties only support set- and index-containers) and processing is aborted.
                    |> S.maybeAndThen (getFromValue "@container")
                        (\containerValue state ->
                            case containerValue of
                                StringValue "@set" ->
                                    state
                                        |> mapDefinition (\definition -> { definition | containerMapping = Just "@set" })
                                        |> S.continue

                                StringValue "@index" ->
                                    state
                                        |> mapDefinition (\definition -> { definition | containerMapping = Just "@index" })
                                        |> S.continue

                                NullValue ->
                                    state
                                        |> mapDefinition (\definition -> { definition | containerMapping = Nothing })
                                        |> S.continue

                                _ ->
                                    InvalidReverseProperty
                                        |> fail "11.4"
                        )
                    -- 11.5) Set the reverse property flag of definition to true.
                    |> S.map (mapDefinition (\definition -> { definition | reverse = True }))
                    -- 11.6) Set the term definition of term in active context to definition and the value associated with defined's key term to true and return.
                    |> S.map setDefinitionInActiveContext
                    |> S.map (setDefined True)
                    |> S.andThen return
            )
        -- 12) Set the reverse property flag of definition to false.
        |> S.map (mapDefinition (\definition -> { definition | reverse = False }))
        -- 13) If value contains the key @id and its value does not equal term:
        |> S.maybeAndThen
            (getFromValue "@id"
                >> Maybe.andThen
                    (\idValue ->
                        if idValue == StringValue term then
                            Nothing

                        else
                            Just idValue
                    )
            )
            (\idValue_ state ->
                case idValue_ of
                    StringValue idValue ->
                        -- 13.2) Otherwise, set the IRI mapping of definition to the result of using the IRI Expansion algorithm, passing active context, the value associated with the @id key for value, true for vocab, false for document relative, local context, and defined. If the resulting IRI mapping is neither a keyword, nor an absolute IRI, nor a blank node identifier, an invalid IRI mapping error has been detected and processing is aborted; if it equals @context, an invalid keyword alias error has been detected and processing is aborted.
                        case expandIRI state.active False True (Just local) (Just state.defined) idValue of
                            Ok expandedIdValue ->
                                if not (String.contains ":" expandedIdValue || List.member expandedIdValue keywords) then
                                    InvalidIRIMapping |> fail "13.2"

                                else if expandedIdValue == "@context" then
                                    InvalidKeywordAlias |> fail "13.2"

                                else
                                    state
                                        |> mapDefinition (\definition -> { definition | mapping = expandedIdValue })
                                        |> S.continue

                            Err e ->
                                e |> S.fail

                    _ ->
                        -- 13.1) If the value associated with the @id key is not a string, an invalid IRI mapping error has been detected and processing is aborted.
                        InvalidIRIMapping
                            |> fail "13.1"
            )
        -- 14)  Otherwise if the term contains a colon (:):
        |> S.ifAndThen
            (\_ -> String.contains ":" term)
            (\state_ ->
                state_ |> S.continue
             -- 14.1) If term is a compact IRI with a prefix that is a key in local context a dependency has been found. Use this algorithm recursively passing active context, local context, the prefix as term, and defined.
             -- TODO
             -- 14.2) If term's prefix has a term definition in active context, set the IRI mapping of definition to the result of concatenating the value associated with the prefix's IRI mapping and the term's suffix.
             -- TODO
             -- 14.3) Otherwise, term is an absolute IRI or blank node identifier. Set the IRI mapping of definition to term.
             -- TODO
            )
        -- 15) Otherwise, if active context has a vocabulary mapping, the IRI mapping of definition is set to the result of concatenating the value associated with the vocabulary mapping and term. If it does not have a vocabulary mapping, an invalid IRI mapping error been detected and processing is aborted.
        |> S.andThen
            (\state ->
                case state.active.vocabularyMapping of
                    Just vocabularyMapping ->
                        state
                            |> mapDefinition (\definition -> { definition | mapping = String.concat [ vocabularyMapping, term ] })
                            |> S.continue

                    Nothing ->
                        InvalidIRIMapping |> fail "15"
            )
        -- 16) If value contains the key @container:
        |> S.maybeAndThen (getFromValue "@container")
            (\containerValue state ->
                -- 16.1) Initialize container to the value associated with the @container key, which must be either @list, @set, @index, or @language. Otherwise, an invalid container mapping error has been detected and processing is aborted.
                case containerValue of
                    StringValue container ->
                        if List.member container [ "@list", "@set", "@index", "@language" ] then
                            -- 16.2) Set the container mapping of definition to container.
                            state
                                |> mapDefinition (\definition -> { definition | containerMapping = Just container })
                                |> S.continue

                        else
                            InvalidContainerMapping
                                |> fail "16"

                    _ ->
                        InvalidContainerMapping
                            |> fail "16"
            )
        -- 17) If value contains the key @language and does not contain the key @type:
        |> S.maybeAndThen
            (\state ->
                case getFromValue "@type" state of
                    Nothing ->
                        getFromValue "@language" state

                    Just _ ->
                        Nothing
            )
            (\languageValue state ->
                case languageValue of
                    -- 17.1) Initialize language to the value associated with the @language key, which must be either null or a string. Otherwise, an invalid language mapping error has been detected and processing is aborted.
                    NullValue ->
                        state
                            |> mapDefinition (\definition -> { definition | languageMapping = Nothing })
                            |> S.continue

                    StringValue language ->
                        -- 17.2) If language is a string set it to lowercased language. Set the language mapping of definition to language.
                        state
                            |> mapDefinition
                                (\definition ->
                                    { definition
                                        | languageMapping = language |> String.toLower |> Just
                                    }
                                )
                            |> S.continue

                    _ ->
                        InvalidLanguageMapping
                            |> fail "17"
            )
        -- 18) Set the term definition of term in active context to definition and set the value associated with defined's key term to true.
        |> S.map setDefinitionInActiveContext
        |> S.map (setDefined True)
        -- Algorithm ends (implicit return)
        |> S.andThen return
        -- Return as Result
        |> S.toResult (\_ -> Err ( [], InvalidLocalContext ))


expandIRI : Context -> Bool -> Bool -> Maybe (AssocList JsonValue) -> Maybe (Dict String Bool) -> String -> Result ( List String, Error ) String
expandIRI active documentRelative vocab maybeLocal maybeDefined value =
    { value = value
    , active = active
    , documentRelative = documentRelative
    , vocab = vocab
    , local = maybeLocal
    , defined = maybeDefined |> Maybe.withDefault Dict.empty
    }
        |> S.continue
        -- 1) If value is a keyword or null, return value as is.
        |> S.andThen
            (\state ->
                if List.member value keywords then
                    value |> S.done

                else
                    state |> S.continue
            )
        -- 2) If local context is not null, it contains a key that equals value, and the value associated with the key that equals value in defined is not true, invoke the Create Term Definition algorithm, passing active context, local context, value as term, and defined. This will ensure that a term definition is created for value in active context during Context Processing.
        |> S.ifAndThen
            (\state -> (Maybe.map (AL.get value) state.local |> isJust) && not (Dict.get value state.defined |> Maybe.withDefault False))
            (\state ->
                case createTermDefinition (state.local |> Maybe.withDefault []) value state.active state.defined of
                    Ok ( active_, defined_ ) ->
                        { state
                            | active = active
                            , defined = defined_
                        }
                            |> S.continue

                    Err e ->
                        e |> S.fail
            )
        -- 3) If vocab is true and the active context has a term definition for value, return the associated IRI mapping.
        |> S.maybeAndThen
            (\state ->
                if vocab then
                    Dict.get value state.active.termDefinitions

                else
                    Nothing
            )
            (\definition _ -> definition.mapping |> S.done)
        -- 4) If value contains a colon (:), it is either an absolute IRI, a compact IRI, or a blank node identifier:
        |> S.maybeAndThen
            (\state ->
                -- 4.1) Split value into a prefix and suffix at the first occurrence of a colon (:).
                IRI.split value
            )
            (\( prefix, suffix ) state_ ->
                state_
                    |> S.continue
                    -- 4.2) If prefix is underscore (_) or suffix begins with double-forward-slash (//), return value as it is already an absolute IRI or a blank node identifier.
                    |> S.andThen
                        (\state ->
                            if prefix == "_" || String.startsWith "//" suffix then
                                state.value |> S.done

                            else
                                state |> S.continue
                        )
                    -- 4.3) If local context is not null, it contains a key that equals prefix, and the value associated with the key that equals prefix in defined is not true, invoke the Create Term Definition algorithm, passing active context, local context, prefix as term, and defined. This will ensure that a term definition is created for prefix in active context during Context Processing.
                    |> S.ifAndThen
                        (\state -> (Maybe.map (AL.get prefix) state.local |> isJust) && not (Dict.get prefix state.defined |> Maybe.withDefault False))
                        (\state ->
                            case createTermDefinition (state.local |> Maybe.withDefault []) prefix state.active state.defined of
                                Ok ( active_, defined_ ) ->
                                    { state
                                        | active = active
                                        , defined = defined_
                                    }
                                        |> S.continue

                                Err e ->
                                    e |> S.fail
                        )
                    -- 4.4) If active context contains a term definition for prefix, return the result of concatenating the IRI mapping associated with prefix and suffix.
                    |> S.maybeAndThen (\state -> Dict.get prefix state.active.termDefinitions)
                        (\definition _ ->
                            String.concat [ definition.mapping, suffix ] |> S.done
                        )
                    -- 4.5) Return value as it is already an absolute IRI.
                    |> S.andThen
                        (\state ->
                            state.value |> S.done
                        )
            )
        -- 5) If vocab is true, and active context has a vocabulary mapping, return the result of concatenating the vocabulary mapping with value.
        |> S.maybeAndThen
            (\state ->
                if state.vocab then
                    state.active.vocabularyMapping

                else
                    Nothing
            )
            (\vocabularyMapping state ->
                String.concat [ vocabularyMapping, state.value ]
                    |> S.done
            )
        -- 6) Otherwise, if document relative is true, set value to the result of resolving value against the base IRI. Only the basic algorithm in section 5.2 of [RFC3986] is used; neither Syntax-Based Normalization nor Scheme-Based Normalization are performed. Characters additionally allowed in IRI references are treated in the same way that unreserved characters are treated in URI references, per section 6.5 of [RFC3987].
        |> S.maybeAndThen
            (\state ->
                if state.documentRelative then
                    state.active.baseIRI

                else
                    Nothing
            )
            (\baseIRI state ->
                -- TODO: resolve against base IRI
                state |> S.continue
            )
        -- 7) Return value as is.
        |> S.andThen (\state -> state.value |> S.done)
        |> Debug.log "expandIRI"
        |> S.toResult (\_ -> Err ( [], InvalidLocalContext ))


isJust : Maybe a -> Bool
isJust maybe =
    maybe |> Maybe.map (\_ -> True) |> Maybe.withDefault False



-- more helpers


{-| Detect if string is a blank node identifier (<https://www.w3.org/TR/json-ld-api/#dfn-blank-node>)
-}
isBlankNodeIdentifier : String -> Bool
isBlankNodeIdentifier =
    String.startsWith "_"


{-| List of JSON-LD keywords
-}
keywords : List String
keywords =
    [ "@context "
    , "@id"
    , "@value"
    , "@language"
    , "@type"
    , "@container"
    , "@list"
    , "@set"
    , "@reverse"
    , "@index"
    , "@base"
    , "@vocab"
    , "@graph"
    ]



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
                        case AL.get "@context" values of
                            Nothing ->
                                JD.succeed empty

                            Just local ->
                                update [] local empty
                                    |> resultToDecoder

                    _ ->
                        JD.succeed empty
            )
