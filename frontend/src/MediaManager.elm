module MediaManager exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import File exposing (File)
import File.Select as Select

import State exposing (..)
import GroupManager exposing (authorizator)

image_list mediastate =
    details []
        [ summary [] [ text "Images known to the system: " ]
        , div [ class "manager-image-container" ]
            (  mediastate.known_metadata
            |> List.map (\meta ->
                             [ h3 [] [ text meta.filename ]
                             , img [ src ("/api/pictures/" ++ meta.id) ] []
                             , div [] [ h5 [] [ text ((String.fromFloat meta.latitude) ++ ", " ++ (String.fromFloat meta.longitude))]]])
            |> List.concat)]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
      
mediamanager state = [ input [ type_ "file"
                               , multiple False
                               , on "change" (D.map GotInputFiles filesDecoder)] []
                       , text "Manageroin mediaa "
                     , image_list state ]

mediaManagerView = authorizator mediamanager
