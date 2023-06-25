module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

          
homeScreen =
    [  p [] [ text "Pichunter is a game where you guess where on a map pictures have been taken"]
    , p [] [ text "Register "
           , a [ href "/register"] [ text "here"]
           , text "!"]]
    
