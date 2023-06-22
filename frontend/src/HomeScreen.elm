module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

loginView loginstate
    =
      [ h2 [] [ text "Welcome to pichunter" ]
      , label [for "username"] [text "Username"]
      , input [ name "username"
              , id "username"
              , onInput LoginUsername
              , value loginstate.username] []
      , label [for "password"] [text "Password"]
      , input [ name "password"
              , id "password"
              , type_ "password"
              , onInput LoginPassword
              , value loginstate.password] []
      , button [ ] [ text "Log in!"]]
      
homeScreen loginstate =
    [ header [ class "login" ]
          (loginView loginstate)
    , p [] [ text "Pichunter is a game where you guess where on a map pictures have been taken"]
    , p [] [ text "Register "
           , a [ href "/register"] [ text "here"]
           , text "!"]]
    
