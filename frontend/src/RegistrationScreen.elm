module RegistrationScreen exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import State exposing (..)
import Pichunter_http exposing (..)

registrationScreen: Maybe RegistrationForm -> List (Html Msg)
registrationScreen maybe_formState =
    case maybe_formState of
        Nothing -> [ div [] [ text "initializing registration state faileD" ]]
        Just formState ->
            let password_match = formState.password == formState.password_again in
            [ div [ class "form" ]
                  [ label [for "displayname"] [text "Name others see you"]
                  , input [ name "displayname"
                          , id "displayname"
                          , onInput RegistrationDisplayname
                          , value formState.displayname ] [  ]
                      
                  , label [for "username"] [text "Username"]
                  , input [ name "username"
                          , onInput RegistrationUsername
                          , id "username"
                          , value formState.username] [ ]
                      
                  , label [for "password"] [text "Password"]
                  , input [ name "password"
                          , id "password"
                          , onInput (RegistrationPassword First)
                          , type_ "password"
                          , value formState.password] []
                      
                  , label [for "password2"] [text "Repeat password"]
                  , input [ name "password"
                          , id "password"
                          , onInput (RegistrationPassword Second)
                          , type_ "password"
                          , value formState.password_again] []
                      
                  , div [ class (if password_match then "" else "danger" )] [ text ("Passwords " ++
                                       (if not password_match then
                                            "don't"
                                        else
                                            "do") ++ " match")]              
                      
                      
                  , input [ type_ "submit"
                          , onClick (SendRegistration formState)
                          ] [ text "Register!"]
                  -- , text (Debug.toString formState)
                  ]]
