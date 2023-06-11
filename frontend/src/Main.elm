module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http
import Url

import File_view
import File exposing (mime)
import State exposing (..)
import Pichunter_http exposing (..)


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

        
init _ url key =
        ( Model File_upload_demo "hello world!"
        , Cmd.batch [] )
        
      
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch 
                  [ ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hello -> ( { model | world = "Update ran!"}
                 , Cmd.none)
        GotInputFiles files ->
            if List.all (\file -> String.startsWith "image" (mime file)) files then
                ( model
                , Cmd.batch (List.map (\file -> postPicture file) files))
            else
                Debug.log ("Expected images, got " ++ (String.join ", " (List.map mime files)))
                ( model
                , Cmd.none)
        UploadedImage result ->
            Debug.log ("Probably uploaded an image. What does the lisp side look like? " ++ (Debug.toString result))
            ( model, Cmd.none)
        UrlChanged _ -> ( model, Cmd.none)
        LinkClicked _ -> ( model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    { title = "Hello pichunter!"
    , body = case model.viewstate of
                 Demo -> [ div [] [ text model.world ]
                         , button [onClick Hello] [ text "TEST ME"]]
                 File_upload_demo -> File_view.view model}
