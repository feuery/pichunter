module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

          
homeScreen session =
    [  p [] [ text "Pichunter is a game where you guess where on a map pictures have been taken"]
    , case session of
          LoggedIn user -> div []
                           [ p [] [ text ("You're logged in, " ++ user.displayName) ]
                           , p [] [ text "Play \"where's that taken at?\" "
                                  , a [ href "/play/locationguessing"] [ text "Here "]]]
          LoggedOut ->
              p [] [ text "Register "
                   , a [ href "/register"] [ text "here"]
                   , text "!"]]
    
