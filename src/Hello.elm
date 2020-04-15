module Hello exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
import Html as H
import RDF
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
    RDF.Triple


init : {} -> Return Msg Model
init flags =
    RDF.triple
        (RDF.subjectIRI "http://example.com/test")
        (RDF.predicateIRI "http://example.com/color")
        (RDF.objectIRI "http://example.com/red")
        |> Return.singleton



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Return Msg Model
update msg model =
    model
        |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> B.Document Msg
view model =
    { title = "Hello World"
    , body = [ model |> Debug.toString |> H.text ]
    }
