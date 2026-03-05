module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, pre, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Json.Encode as Encode
import Html.Attributes exposing (style)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { text : String
    , posts : List Post
    }


type alias Post =
    { id : Int
    , text : String
    }


postDecoder : Decoder Post
postDecoder =
    map2 Post
        (field "id" int)
        (field "text" string)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { text = "", posts = [] }
    , Http.get
        { url = "http://localhost:8080/posts"
        , expect = Http.expectJson GotPosts (list postDecoder)
        }
    )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error String)
    | Change String
    | SendText String
    | GotPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse _ ->
            ( model, Cmd.none )

        GotPosts (Ok ps) ->
            ( { model | posts = ps }, Cmd.none )

        GotPosts (Err e) ->
            let
                _ =
                    Debug.log "err" e
            in
            ( model, Cmd.none )

        Change text ->
            ( { model | text = text }, Cmd.none )

        SendText text ->
            ( { model | text = "" }
            , Http.post
                { url = "http://localhost:8080/posts"
                , expect = Http.expectString GotResponse
                , body = Http.jsonBody (Encode.object [ ( "text", Encode.string text ) ])
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ div []
            [ viewInput "text" "Text" model.text
            , button [ onClick (SendText model.text) ] [ text "Post" ]
            ]
        , viewPosts model.posts
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div [] (List.map viewPost (List.reverse posts))


viewPost : Post -> Html Msg
viewPost post =
    div [] [ text (String.fromInt post.id), text " ", text post.text ]


viewInput : String -> String -> String -> Html Msg
viewInput t p v =
    input [ type_ t, placeholder p, value v, onInput Change ] []
