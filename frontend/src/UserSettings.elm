module UserSettings exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import User exposing (User)
import Json.Decode as Decode

import State exposing (..)
import MediaManager



editor user formstate =
    [ p [] [ text "Fill the old password only when setting a new password"]
    , ul [ class "usersettings" ]
        ([ ("Login username", "login_usrname", formstate.username)
        , ( "Display name", "display_name", formstate.displayname)
        , ( "Old password", "old_passwd", formstate.oldPassword)
        , ( "Password", "passwd", formstate.newPassword)]
        |> List.concatMap ( \tuple ->
                                let (title, input_id, value_) = tuple in
                                [ li [class "user-setting-list-item"] [ h3 [] [ label [ id (input_id ++ "_lbl")
                                                                                      , for input_id ]
                                                                                    [ text title]]]
                                , li [class "user-setting-list-item"] [ input
                                                                            [ onInput (ChangeUserField formstate title)
                                                                            , id input_id
                                                                            , type_
                                                                                  (if String.contains "password" (String.toLower title) then
                                                                                       "password"
                                                                                   else
                                                                                       "text")
                                                                            , value value_] []]]))
    , ul [ class "usersettings" ]
        [ li [ class "user-setting-list-item"] [ h3 [] [ text "Picture" ]] 
        , li [ class "user-setting-list-item"] [ input [ type_ "file"
                                                       , accept "image/*"
                                                       , multiple False
                                                       , id "avatar_file" 
                                                       , on "change" (Decode.map GotUserFile MediaManager.filesDecoder)] []]]

    , button [ id "save_user_btn"
             , onClick (SaveLoggedInUser user formstate) ] [text "Save user"]]

defaultFormState: User -> Maybe UserSettingsFormState -> UserSettingsFormState
defaultFormState user formstate =
    Maybe.withDefault ( UserSettingsFormState
                            user.username
                            user.displayName
                            "" "" Nothing) formstate
view session formstate = 
    case session of
            LoggedIn user ->
                let state = defaultFormState user formstate  in
                editor user state
            LoggedOut ->
                [ div [] [ text "Can't set up your settings when you're not logged in"]]
