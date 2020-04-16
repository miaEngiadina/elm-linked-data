module OpenEngiadina exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as JD
import RDF
import RDF.JSON
import Return exposing (Return)


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
    Result String RDF.Graph


init : {} -> Return Msg Model
init flags =
    "Loading ... "
        |> Result.Err
        |> Return.singleton
        |> Return.command
            (Http.get
                { url = "https://openengiadina.net/public"
                , expect = Http.expectJson Receive RDF.JSON.decoder
                }
            )



-- UPDATE


type Msg
    = Receive (Result Http.Error RDF.Graph)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Receive result ->
            result
                |> Result.mapError Debug.toString
                |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


type alias Note =
    { iri : RDF.IRI
    , content : RDF.Literal
    }


activityStreams : String -> RDF.IRI
activityStreams =
    RDF.namespace "https://www.w3.org/ns/activitystreams#"


noteFromDescription : RDF.Description -> Maybe Note
noteFromDescription description =
    Maybe.map2 Note
        -- (RDF.descriptionGet
        --     (activityStreams "to" |> RDF.predicateIRI)
        --     description
        --     |> Debug.log "afterdescriptionGet to"
        --     |> List.filterMap RDF.asIRI
        --     |> Just
        -- )
        -- (RDF.descriptionGet
        --     (activityStreams "attributedTo" |> RDF.predicateIRI)
        --     description
        --     |> List.filterMap RDF.asIRI
        --     |> List.head
        -- )
        (.subject description |> RDF.asIRI)
        (RDF.descriptionGet
            (activityStreams "content" |> RDF.predicateIRI)
            description
            |> List.filterMap RDF.asLiteral
            |> List.head
        )


getNotes : RDF.Graph -> List Note
getNotes graph =
    graph
        |> RDF.filterForDescriptions
            (\{ subject, predicate, object } ->
                (predicate == RDF.type_)
                    && object
                    == ("Note"
                            |> activityStreams
                            |> RDF.objectIRI
                       )
            )
        |> List.filterMap noteFromDescription


graphView : RDF.Graph -> H.Html Msg
graphView graph =
    H.table []
        (List.map
            (\{ subject, predicate, object } ->
                H.tr []
                    [ subject |> Debug.toString |> H.text |> List.singleton |> H.td []
                    , predicate |> Debug.toString |> H.text |> List.singleton |> H.td []
                    , object |> Debug.toString |> H.text |> List.singleton |> H.td []
                    ]
            )
            graph
        )


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
            getNotes graph
    in
    H.div [] (notes |> List.map noteView)


view : Model -> B.Document Msg
view model =
    { title = "Notes from openEngiadina"
    , body =
        [ H.header []
            [ H.h1 [] [ H.text "Notes from openEngiadina" ]
            , H.p [] [ H.text "This is a small example app that fetches public ActivityPub Notes from an ActivityPub server at https://openengiadina.net/" ]
            ]
        , case model of
            Ok graph ->
                H.main_ []
                    [ notesView graph
                    ]

            Err err ->
                err |> Debug.toString |> H.text
        ]
    }
