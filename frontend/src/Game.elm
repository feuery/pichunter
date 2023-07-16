module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import File exposing (File)
import File.Select as Select

import State exposing (..)
import MediaManager exposing (map_id_to_element_id)

authorizator view session state =
    case session of
        LoggedIn _ ->
            view session state
        LoggedOut ->
            [ div [] [ text "Unauthorized" ] ]

actual_gameview session gamestate =
    [ div [ class "w"]
          [ h3 [] [ text "Where is this picture taken at?" ]
          , div [ class "grid-container" ]
                          [ img [ class "picture"
                                , src ("/api/pictures/" ++ gamestate.next_pic.id) ] []
                          , let map_id = map_id_to_element_id gamestate.next_pic.id in
                            div [ id map_id
                                , class "map" ] []]]]
           

gameview = authorizator actual_gameview
