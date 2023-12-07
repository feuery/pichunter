module GroupManager exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import State exposing (..)

list_diff a b =
    ( a
    |> List.filter (\aa ->
                        not (List.member aa b)))

userDetails user =
    div []
        [ h5 [] [ text "User details"]
        , div [class "userdetails" ]
            [ label [ id "username_lbl"
                    , for "username" ] [ text "Username" ]
            , input [ id "username"
                    , disabled True
                    , value user.username] []
            , label [ id "displayname_lbl"
                    , for "displayname"] [ text "Display name" ]
            , input [ id "displayname"
                    , onInput (AdminSetUsername user)
                    , value user.displayName] []
                
            , case user.imgId of
                  Just img_id ->
                      img [src ("/api/pictures/" ++ img_id)] []
                  _ ->
                      p [] [ text "no image set up"]
            , label [ for "activated"
                    , id "activated_lbl"]
                  [ text "User activated?"]
            , input [ id "activated" 
                    , type_ "checkbox"
                    , onCheck (AdminUserActivated user)
                    , checked user.activated] []
            , label [ for "banned"
                    , id "banned_lbl"]
                  [ text "User banned?"]
            , input [ id "banned"
                    , type_ "checkbox"
                    , onCheck (AdminUserBanned user)
                    , checked user.banned] []]]
        
all_users users =
    div [] [ h5 [] [text "All users"]
           , select [ multiple True
                    , onInput AdminUserSelected]
               (List.map (\user ->
                              (option [ value (String.fromInt user.id) ]
                                   [ text (user.displayName ++ "(" ++ user.username ++ ")")]))
                    users)
           , button [ onClick AdminUserToGroup] [text "Move user to the selected group"]]
        
group_users groupstate group =
    div [] [ h5 [] [text "Users of the group"]
           , select [ multiple True
                    , onInput AdminUserSelected]
               (List.map (\user ->
                              (option [ value (String.fromInt user.id) ]
                                   [ text (user.displayName ++ "(" ++ user.username ++ ")")]))
                    group.users)
           , button [ onClick AdminUserFromGroup] [ text "Drop user from the selected group"]]

user_container state selectedGroup =
    div [ class "group_container" ]
        [ group_users state selectedGroup
        , div [] [text ""]
        , all_users (list_diff selectedGroup.all_users selectedGroup.users) ]
        
group_permissions group =
    div [ class "group_container" ]
        [ div [] [ h5 [] [text "Group's abilities"]
                 , (select [ multiple True
                           , onInput AdminSelectExistingAbility]
                        ( group.permissions
                        |> List.filter (\g -> g.id /= Nothing)
                        |> List.map (\g ->
                                         let id = (Maybe.withDefault -1 g.id) in
                                         (option [ value (String.fromInt id)]
                                              [ text (Maybe.withDefault "" g.action) ]))))]
                                                  
        , button [ onClick AdminDisallow ] [ text "disallow group to" ]
        
        , let all_abilities = group.all_abilities
              group_abilities = group.permissions in
          div []
              [  h5 [] [text "Group can't do: "]
              , (select [ multiple True
                        , onInput AdminSelectNonExistingAbility ]
                     (  list_diff all_abilities group_abilities
                     |> List.filter (\perm -> perm.id /= Nothing)
                     |> List.map (\permission ->
                                      let id = (Maybe.withDefault -1 permission.id) in
                                      option [ value (String.fromInt id) ]
                                      [ text (Maybe.withDefault "" permission.action)])))]
        , button [ onClick AdminAllow ] [ text "allow group to" ]]
                   
                    
group_details group =
    div [] [ -- text (Debug.toString group)
           ]
        
groupmanager groupstate =
    case groupstate of
        Just state ->
            let groups = state.loadedGroups in
            [ div [] [ h5 [] [text "Groups"]
                     , select [ multiple True
                              , onInput AdminGroupSelected]
                           (List.map (\group ->
                                          (option [ value (String.fromInt group.id)]
                                               [ text group.name ]))
                                groups)
                     , ul [ class "groupmanager" ]
                         [ li [] [ case state.selectedGroup of
                                       Just selectedGroup -> group_details selectedGroup
                                       _ -> div [] []]
                         , li [] [ case state.selectedGroup of
                                       Just selectedGroup -> group_permissions selectedGroup
                                       _ -> div [] []]
                         , li [] [ case state.selectedGroup of
                                       Just selectedGroup -> user_container state selectedGroup
                                       _ -> p [] []]]]
            ,  case state.selectedUser of
                   Just user -> userDetails user
                   _ -> div [] []
            , button [ onClick SaveGroupManagerState ] [ text "Save groups"]
            -- , case state.selectedPermission of
            --       Just permission ->
            --           div [] [text (Debug.toString permission)]
            --       _ -> div [] [text "no permission selected"]
            ]
        Nothing -> [ div [] [ text "Manager state is uninitialized" ]]

authorizator view ability groupstate session =
    case session of
        LoggedIn user ->
            if List.member ability user.abilities then
                view groupstate
            else [ div [] [ text "you need \"can-admin\" ability" ] ]
        LoggedOut ->
            [ div [] [ text "Not authorized" ]]

                

groupManagerView = authorizator groupmanager "can-admin"
