module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http

import Url


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


type Msg
    = Hello
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
        
type alias Model =
    { world: String }


init _ url key =
        ( Model "hello world!"
        , Cmd.batch [] )
        
      
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch 
                  [ ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hello -> ( { model | world = "Update ran!"}
                 , Cmd.none)
        UrlChanged _ -> ( model, Cmd.none)
        LinkClicked _ -> ( model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    { title = "Hello pichunter!"
    , body =
        [ div [] [ text model.world ]
        , button [onClick Hello] [ text "TEST ME"]]}
