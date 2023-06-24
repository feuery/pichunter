module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

loginView session loginstate
    = ( h2 [] [ text "Welcome to pichunter" ] )
          ::
      (case session of
           LoggedIn usr -> [ span [] [ text ("Welcome, " ++ usr.displayName)]
                           , button [ onClick Logout ] [ text "Log out"]]
           LoggedOut -> [ label [for "username"] [text "Username"]
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
                        , button [ onClick (Login loginstate)] [ text "Log in!"]])
          
homeScreen session loginstate =
    [ header [ class "login" ]
          (loginView session loginstate)
    , p [] [ text "Pichunter is a game where you guess where on a map pictures have been taken"]
    , p [] [ text "Register "
           , a [ href "/register"] [ text "here"]
           , text "!"]]
    
