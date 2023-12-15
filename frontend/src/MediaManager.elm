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

len_str lst = String.fromInt (List.length lst)

image meta =
    let map_id = map_id_to_element_id meta.id
        approved = (meta.approver /= Nothing) in
    li []
        [ details []
              [summary [] [ h5 [] [ text meta.filename ]]
              , div [ class "grid-container" ]
                  [ img [ class "picture"
                        , src ("/api/pictures/" ++ meta.id) ] []
                  , div [ id map_id
                        , class "map" ] []
                  , div [ class "mediamanager_toolbox"]
                      (List.append [ button [ onClick (RemovePicture meta) ] [text "Remove this picture"]
                                   , label [ for "approved"] [ text "Approved? "] 
                                   , input [ type_ "checkbox"
                                           , checked approved
                                           , onCheck (ApproveImage meta)
                                           , id "approved"] []]
                           (case meta.approver of
                               Just approver ->
                                   [ text ("Approved by " ++ approver.displayName)]
                               Nothing -> [ ]))]
              , div []
                  [ text (Debug.toString meta)]]]
              
image_list mediastate =
    details [] [ summary [] [h3 [] [ text ("Images known to the system (" ++ (len_str mediastate.known_metadata) ++ ")")]]
                 
           , ul [ class "manager-image-container" ]
               (  mediastate.known_metadata
               |> List.map image)]

unapproved_list mediastate =
    details [] [ summary [] [ h3 [] [ text ("Unapproved images (" ++ len_str mediastate.unapproved_queue ++ ")")]]
               , ul [ class "manager-image-container" ]
                   (  mediastate.unapproved_queue
                   |> List.map image)]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
      
mediamanager state = [ image_list state
                     , unapproved_list state
                     , h3 [] [ text "Input new pictures: "]
                     , input [ type_ "file"
                             , accept "image/*"
                             , multiple False
                             , id "mediamanager_input"
                             , on "change" (D.map GotInputFiles filesDecoder)] []]

mediaManagerView = authorizator mediamanager "can-admin"
