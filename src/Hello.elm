module Hello exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
import Html as H
import Http
import Json.Decode
import Json.Value
import RDF
import Return exposing (Return)
import Json.LD.Context as Context


main : Program {} Model Msg
main =
    B.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getExampleContext : Cmd Msg
getExampleContext =
    { url = "https://w3c.github.io/json-ld-api/tests/context.jsonld"
    , expect = Http.expectString ReceiveContext
    }
        |> Http.get



-- MODEL


type alias Model =
    String


init : {} -> Return Msg Model
init flags =
    RDF.triple
        (RDF.subjectIRI "http://example.com/test")
        (RDF.predicateIRI "http://example.com/color")
        (RDF.objectIRI "http://example.com/red")
        |> Debug.toString
        |> Return.singleton
        |> Return.command getExampleContext



-- UPDATE


type Msg
    = ReceiveContext (Result Http.Error String)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ReceiveContext (Ok context) ->
            context
                |> Json.Decode.decodeString Context.decoder
                |> Debug.toString
                |> Return.singleton

        ReceiveContext (Err e) ->
            e
                |> Debug.toString
                |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> B.Document Msg
view model =
    { title = "Hello World"
    , body = [ model |> H.text ]
    }
