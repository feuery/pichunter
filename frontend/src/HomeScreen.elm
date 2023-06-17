module HomeScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

loginView -- loginstate
    =
    let actual_view = [label [for "username"] [text "Username"],
                       input [name "username", id "username" --, onInput ChangeUsername, onFocus LoginFocus
                             ] [],
                       label [for "password"] [text "Password"],
                       input [name "password", id "password", type_ "password" --, onInput ChangePassword
                             ] []
                      ] in
    div [] -- case loginstate of
            --                       LoggedIn usr ->
            --                           [text ("Welcome, " ++ usr.nickname)]
            --                       LoggingIn username password ->
            --                           (List.concat [actual_view,
            --                                         [button [onClick DoLogIn] [text "Login!"]]])
            --                       LoggedOut ->
        actual_view 
                                  -- LoginFailed ->
                                  --     (List.concat [actual_view,
                                  --                   [button [onClick DoLogIn] [text "Login!"],
                                  --                    div [] [text "Login failed! Check username and password!"]]]))
                

homeScreen =
    [ h2 [] [ text "Welcome to pichunter" ]
    , p [] [ text "Pichunter is a game where you guess where on a map pictures have been taken"]
    , p [] [ text "Register "
           , a [ href "/register"] [ text "here"]
           , text "!"]
    , loginView]
