module Hello exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
import Html as H
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
    String


init : {} -> Return Msg Model
init flags =
    "Loading ... "
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
        Receive (Ok graph) ->
            graph
                |> Debug.toString
                |> Return.singleton

        Receive (Err err) ->
            err
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
