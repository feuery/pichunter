module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

import StupidTime

session_label game_session =
    let correct_guesses = (String.fromInt
                               ( List.length
                                     (List.filter (\guess -> guess.correctly_guessed)
                                          game_session.guesses)))
        all_guesses = (String.fromInt
                               ( List.length
                                     game_session.guesses))
    in
    "Started playing on " ++ (StupidTime.format StupidTime.Date game_session.started_at) ++ " with score " ++ correct_guesses ++ "/" ++ all_guesses
          
homeScreen model =
    let session = model.session in
    [  p [] [ text "Pichunter is a location-based image game" ]
    , case session of
          LoggedIn user -> ul []
                           [ li [] [ a [ href "/play/locationguessing"] [ text "Place pictures on the map"]
                                   , p [] [ text (case model.locationGameSession of
                                                      Just game_session -> 
                                                          session_label game_session
                                                      Nothing ->
                                                          "Didn't find session data")]]
                           , li [] [ a [href "/play/pictureguessing"] [ text "Show pichunter pictures from these places " ]
                                   , p [] [ text (case model.picGameSession of
                                                      Just game_session -> 
                                                          session_label game_session
                                                      Nothing ->
                                                          "Didn't find session data")]]]
          LoggedOut ->
              p [] [ text "Register "
                   , a [ href "/register"] [ text "here"]
                   , text "!"]]
    
