module Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import State exposing (..)

topbar session loginstate
    = header [ class "login" ]
       (  h2 [] [ text "Welcome to pichunter" ]
       :: (case session of
              LoggedIn usr -> [ span [ id "topbar_welcome" ] [ text ("Welcome, " ++ usr.displayName)]
                              , ul [ id "topbar_tools"]
                                  [ li [] [ a [href "/admin/usersgroups"] [ text "Handle users and groups"]]
                                  , li [] [ a [href "/admin/media"] [ text "Handle media"]]]
                                      
                              , button [ onClick Logout
                                       , id "logout_btn" ] [ text "Log out"]]
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
                           , button [ onClick (Login loginstate)] [ text "Log in!"]]))
