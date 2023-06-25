module GroupManager exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import State exposing (..)

groupManagerView session =
    case session of
        LoggedIn user ->
            if List.member "can-admin" user.abilities then
                [ div [] [ text "moro" ] ]
            else [ div [] [ text "you need \"can-admin\" ability" ] ]
        LoggedOut ->
            [ div [] [ text "Not authorized" ]]
