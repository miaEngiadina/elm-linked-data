module Hello exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Debug
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode
import Json.Encode
import Json.LD
import Json.LD.Context as Context
import Json.Value
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
    { input : String
    , context : String
    , expanded : String
    }


testInput =
    """
{
 "@context":
 {
    "name": "http://xmlns.com/foaf/0.1/name",
    "homepage": {
      "@id": "http://xmlns.com/foaf/0.1/homepage",
      "@type": "@id"
    }
 },
 "name": "Manu Sporny",
 "homepage": "http://manu.sporny.org/"
}
    """


init : {} -> Return Msg Model
init flags =
    { input = testInput
    , context = ""
    , expanded = ""
    }
        |> update Decode



-- UPDATE


type Msg
    = Decode
    | UpdateInput String


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Decode ->
            { model
                | context =
                    model.input
                        |> Json.Decode.decodeString Context.decoder
                        |> Debug.toString
                , expanded =
                    case model.input |> Json.Decode.decodeString Json.Value.decoder |> Result.map (Json.LD.expand Context.empty) of
                        Ok expanded ->
                            expanded
                                  |> Json.Value.encode
                                  |> Json.Encode.encode 4

                        Err e ->
                            e |> Json.Decode.errorToString
            }
                |> Return.singleton

        UpdateInput input ->
            { model | input = input }
                |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> B.Document Msg
view model =
    { title = "Hello World"
    , body =
        [ H.div []
            [ H.textarea
                [ HA.style "height" "500px"
                , HA.style "width" "25vw"
                , HE.onInput UpdateInput
                ]
                [ model.input |> H.text ]
            , H.textarea
                [ HA.style "height" "500px"
                , HA.style "width" "25vw"
                , HA.readonly True
                ]
                [ model.context |> H.text ]
            , H.textarea
                [ HA.style "height" "500px"
                , HA.style "width" "25vw"
                , HA.readonly True
                ]
                [ model.expanded |> H.text ]
            ]
        , H.button [ HE.onClick Decode ] [ H.text "Decode" ]
        ]
    }
