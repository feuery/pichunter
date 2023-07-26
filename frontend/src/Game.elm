module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import File exposing (File)
import File.Select as Select

import State exposing (..)
import MediaManager exposing (map_id_to_element_id, filesDecoder)

authorizator view session state =
    case session of
        LoggedIn _ ->
            view session state
        LoggedOut ->
            [ div [] [ text "Unauthorized" ] ]

counties =
    [ ("1", "Uusimaa")
    , ("2", "Varsinais-Suomi")
    , ("4", "Satakunta")
    , ("5", "Kanta-Häme")
    , ("6", "Pirkanmaa")
    , ("7", "Päijät-Häme")
    , ("8", "Kymenlaakso")
    , ("9", "Etelä-Karjala")
    , ("10", "Etelä-Savo")
    , ("11", "Pohjois-Savo")
    , ("12", "Pohjois-Karjala")
    , ("13", "Keski-Suomi")
    , ("14", "Etelä-Pohjanmaa")
    , ("15", "Pohjanmaa")
    , ("16", "Keski-Pohjanmaa")
    , ("17", "Pohjois-Pohjanmaa")
    , ("18", "Kainuu")
    , ("19", "Lappi")
    , ("21", "Ahvenanmaa")]

actual_guessing_gameview session gamestate =
    case gamestate of                     
        LocationGuessingState maybe_meta score tries county ->
            case maybe_meta of
                Just meta ->
                    [ h3 [] [ text "Where is this picture taken at?" ]
                    , div [ class "grid-container" ]
                        [ img [ class "picture"
                              , src ("/api/pictures/" ++ meta.id) ] []
                        , let map_id = map_id_to_element_id meta.id in
                          div [ id map_id
                              , class "map" ] []]
                    , h3 [] [ text "Score: " ]
                    , p [] [ text ((String.fromInt score) ++ "/" ++ (String.fromInt tries))]]
                Nothing ->
                    [ text "No image loaded yet" ]
        _ -> [div [] [ text ("wtf how do you even end up in actual_guessing_gameview with state " ++ (Debug.toString gamestate))]]

actual_picture_gameview session state =
    case state of                     
        PictureGuessingState maybe_meta score tries county allow_for_usage _->
            case maybe_meta of
                Just meta ->
                    [ h3 [] [ text "Give me a picture located here" ]
                    , div [ class "grid-container" ]
                        [ img [ class "picture"
                              , src ("/api/pictures/" ++ meta.id) ] []]
                    , h5 [] [ text "Your file: " ]
                    , input [ type_ "file"
                            , accept "image/*"
                            , multiple False
                            , id "game_file"
                            , on "change" (D.map GotGameFiles filesDecoder)] []
                    , div []
                        [ input [ type_ "checkbox"
                                , checked allow_for_usage
                                , onCheck SetAllowForUsage
                                , id "allow_for_use"] []
                        , label [ for "allow_for_use"] [ text "Allow pichunter to use this picture for questions" ]]
                    , button [ onClick SubmitGuess] [ text "Submit your guess"]
                    , h3 [] [ text "Score: " ]
                    , p [] [ text ((String.fromInt score) ++ "/" ++ (String.fromInt tries))]]
                Nothing ->
                    [ text "No image loaded yet" ]
        _ -> [div [] [ text ("wtf how do you even end up in actual_guessing_gameview with state " ++ (Debug.toString state))]]

gameview_guessing = authorizator actual_guessing_gameview
gameview_pictures = authorizator actual_picture_gameview

choose_county gametype =
    [ h3 [] [ text "Choose a county you expect pictures from" ]
    , select [ onInput (ChoseCounty gametype) ]
        (  ("null", "Choose a county") :: counties
        |> List.map (\c ->
                         let (v, t) = c in
                         option [ value v ]
                         [ text t]))]
    
gameview session gamestate =
    case gamestate of
        ChoosingCounty gametype ->
            choose_county gametype
        LocationGuessingState _ _ _ _ -> gameview_guessing session gamestate
        PictureGuessingState _ _ _ _ _ _ -> gameview_pictures session gamestate
        NotPlaying -> [ div [] [ text "Not playing"] ]
