module OpenEngiadina exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import RDF
import RDF.Decode
import RDF.JSON
import Return exposing (Return)



-- Config


publicUrl : String
publicUrl =
    "http://localhost:4000/public"



-- "https://openengiadina.net/public"


main : Program {} Model Msg
main =
    B.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { graph : RDF.Graph
    , content : String
    }


init : {} -> Return Msg Model
init flags =
    { graph = RDF.fromList []
    , content = ""
    }
        |> Return.singleton
        |> Return.command getNotes


getNotes : Cmd Msg
getNotes =
    Http.get
        { url = publicUrl
        , expect = Http.expectJson Receive RDF.JSON.decoder
        }



-- UPDATE


type Msg
    = Receive (Result Http.Error RDF.Graph)
    | UpdateContent String
    | Post
    | Refresh


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Receive (Ok graph) ->
            graph
                |> (\graph_ -> { model | graph = graph_ })
                |> Return.singleton

        Receive (Err e) ->
            model
                |> Return.singleton

        UpdateContent s ->
            { model | content = s }
                |> Return.singleton

        Post ->
            model
                |> Return.singleton
                |> Return.command (postNote model.content)

        Refresh ->
            model
                |> Return.singleton
                |> Return.command getNotes



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- ActivityStreams Notes


activityStreams : String -> RDF.IRI
activityStreams =
    RDF.namespace "https://www.w3.org/ns/activitystreams#"


type alias Note =
    { iri : RDF.IRI
    , content : RDF.Literal
    }


noteDecoder : RDF.Decode.Decoder Note
noteDecoder =
    RDF.Decode.succeed Note
        |> RDF.Decode.ignore (RDF.Decode.ensureType <| activityStreams "Note")
        |> RDF.Decode.apply RDF.Decode.iriDecoder
        |> RDF.Decode.apply
            (RDF.Decode.objectsDecoder (activityStreams "content" |> RDF.predicateIRI)
                RDF.Decode.literalDecoder
                |> RDF.Decode.first
            )


getNotesFromGraph : RDF.Graph -> List Note
getNotesFromGraph graph =
    graph
        |> RDF.Decode.decodeAll noteDecoder



-- VIEW


noteView : Note -> H.Html Msg
noteView note =
    H.div []
        [ H.a
            [ HA.href (note |> .iri)
            , HA.target "_blank"
            ]
            [ H.h3 [] [ note |> .iri |> H.text ] ]
        , H.pre [] [ note |> .content |> .value |> H.text ]
        ]


notesView : RDF.Graph -> H.Html Msg
notesView graph =
    let
        notes =
            getNotesFromGraph graph
    in
    H.div [] (notes |> List.map noteView)


postNote : String -> Cmd Msg
postNote content =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" "Basic YWxpY2U6MTIz" ]
        , url = "http://localhost:4000/users/alice/outbox"
        , body = Http.stringBody "application/rdf+json" (encodeNote content)
        , expect = Http.expectWhatever (always Refresh)
        , timeout = Nothing
        , tracker = Nothing
        }


encodeNote : String -> String
encodeNote content =
    let
        object_id =
            "object"
                |> RDF.blankNode

        activity_id =
            "activity"
                |> RDF.blankNode
                |> RDF.subjectBlankNode
    in
    RDF.empty
        -- activity
        |> RDF.addTriple
            (RDF.Triple activity_id RDF.type_ (activityStreams "Create" |> RDF.objectIRI))
        |> RDF.addTriple
            (RDF.Triple activity_id (activityStreams "to" |> RDF.predicateIRI) ("http://localhost:4000/users/alice" |> RDF.objectIRI))
        |> RDF.addTriple
            (RDF.Triple activity_id (activityStreams "to" |> RDF.predicateIRI) (activityStreams "Public" |> RDF.objectIRI))
        |> RDF.addTriple
            (RDF.Triple activity_id (activityStreams "object" |> RDF.predicateIRI) (object_id |> RDF.objectBlankNode))
        -- object
        |> RDF.addTriple
            (RDF.Triple (object_id |> RDF.subjectBlankNode)
                RDF.type_
                (activityStreams "Note" |> RDF.objectIRI)
            )
        |> RDF.addTriple
            (RDF.Triple (object_id |> RDF.subjectBlankNode)
                (activityStreams "content" |> RDF.predicateIRI)
                (RDF.literal content (RDF.xsd "string") Nothing
                    |> RDF.objectLiteral
                )
            )
        |> RDF.JSON.encode
        |> JE.encode 2


composeNote : String -> H.Html Msg
composeNote content =
    H.div []
        [ H.input
            [ HA.value content
            , HE.onInput UpdateContent
            ]
            []
        , H.button [ HE.onClick Post ] [ H.text "Post" ]
        , H.p []
            [ "An encoded activity with the note looks like this (encoded as RDF/JSON):"
                |> H.text
            ]
        , H.code []
            [ H.pre []
                [ encodeNote content
                    |> H.text
                ]
            ]
        ]


view : Model -> B.Document Msg
view model =
    { title = "Notes from openEngiadina"
    , body =
        [ H.header []
            [ H.h1 [] [ H.text "Notes from openEngiadina" ]
            , H.p [] [ H.text "This is a small example app that fetches public ActivityPub Notes from an ActivityPub server at https://openengiadina.net/" ]
            ]
        , H.main_
            []
            [ H.h2 [] [ H.text "Compose" ]
            , composeNote model.content
            , H.h2 [] [ H.text "Notes" ]
            , notesView model.graph
            ]
        ]
    }
