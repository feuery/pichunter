module MediaManager exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import File exposing (File)
import File.Select as Select

import State exposing (..)
import GroupManager exposing (authorizator)

map_id_to_element_id id = "map"++(String.replace "-" "" id)

image_list mediastate =
    div [] [ h3 [] [ text "Images known to the system: " ]
           , ul [ class "manager-image-container" ]
               (  mediastate.known_metadata
               |> List.map (\meta ->
                                let map_id = map_id_to_element_id meta.id in
                                li []
                                [ details []
                                      [summary [] [ h5 [] [ text meta.filename ]]
                                      , div [ class "grid-container" ]
                                          [ img [ class "picture"
                                                , src ("/api/pictures/" ++ meta.id) ] []
                                          , div [ id map_id
                                                , class "map" ] []
                                          , div [ class "mediamanager_toolbox"]
                                              [ button [ onClick (RemovePicture meta) ] [text "Remove this picture"]]] ]]))]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
      
mediamanager state = [ image_list state
                     , h3 [] [ text "Input new pictures: "]
                     , input [ type_ "file"
                             , accept "image/*"
                             , multiple False
                             , on "change" (D.map GotInputFiles filesDecoder)] []]

mediaManagerView = authorizator mediamanager "can-admin"
