module File_view exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import State exposing (..)
import File exposing (File)

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)

view model =
    [ h3 [] [ text "Testing the picture upload thing"]
    , label [ for "file-pictures-input" ]
        [ text "Add pictures from device"]
    , input [ type_ "file"
            , multiple False
            , style "display" "none"
            , id "file-pictures-input"
            , on "change" (D.map GotInputFiles filesDecoder)] []]
