module Hello exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Html as H
import Return exposing (Return)
import Debug
import Rdf


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
    Rdf.Triple


init : {} -> Return Msg Model
init flags =
    Rdf.triple
        (Rdf.subjectIRI "http://example.com/test")
        (Rdf.predicateIRI "http://example.com/color")
        (Rdf.objectIRI "http://example.com/red")
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
