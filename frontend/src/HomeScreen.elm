module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

          
homeScreen session =
    [  p [] [ text "Pichunter is a location-based image game" ]
    , case session of
          LoggedIn user -> ul []
                           [ li [] [ a [ href "/play/locationguessing"] [ text "Place pictures on the map"]
                                   , p [] [ text "Tähän sit jotain dataa kesken olevasta pelistä"] ]
                           , li [] [ a [href "/play/pictureguessing"] [ text "Show pichunter pictures from these places " ]
                                   , p [] [ text "Tähän sit jotain dataa kesken olevasta pelistä"]]]
          LoggedOut ->
              p [] [ text "Register "
                   , a [ href "/register"] [ text "here"]
                   , text "!"]]
    
