module GroupManager exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import State exposing (..)

list_diff a b =
    ( a
    |> List.filter (\aa ->
                        not (List.member aa b)))

userDetails user =
    div [] [ text (Debug.toString user)]

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

group_permissions group =
    div [] [ h5 [] [text "Group's abilities"]
           , (select [ multiple True
                     , onInput AdminSelectExistingAbility]
                  ( group.permissions
                  |> List.filter (\g -> g.id /= Nothing)
                  |> List.map (\g ->
                                   let id = (Maybe.withDefault -1 g.id) in
                                   (option [ value (String.fromInt id)]
                                        [ text (Maybe.withDefault "" g.action) ]))))
              
           , button [ onClick AdminDisallow ] [ text "disallow group to" ]

               
           , h5 [] [text "Group can't do: "]
           , let all_abilities = group.all_abilities
                 group_abilities = group.permissions in 
                 (select [ multiple True
                         , onInput AdminSelectNonExistingAbility ]
                  (  list_diff all_abilities group_abilities
                  |> List.filter (\perm -> perm.id /= Nothing)
                  |> List.map (\permission ->
                                  let id = (Maybe.withDefault -1 permission.id) in
                                  option [ value (String.fromInt id) ]
                                  [ text (Maybe.withDefault "" permission.action)])))
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
                     , case state.selectedGroup of
                           Just selectedGroup ->
                               div []
                                   [ group_details selectedGroup
                                   , group_permissions selectedGroup]
                           _ -> div [] []]
            , case state.selectedGroup of
                  Just selectedGroup ->
                      div [] [ group_users state selectedGroup
                             , all_users (list_diff selectedGroup.all_users selectedGroup.users) ]
                  _ -> p [] []
            , button [] [ text "Save groups"]
            , case state.selectedPermission of
                  Just permission ->
                      div [] [text (Debug.toString permission)]
                  _ -> div [] [text "no permission selected"]]
        Nothing -> [ div [] [ text "Manager state is uninitialized" ]]

authorizator view groupstate session =
    case session of
        LoggedIn user ->
            if List.member "can-admin" user.abilities then
                view groupstate
            else [ div [] [ text "you need \"can-admin\" ability" ] ]
        LoggedOut ->
            [ div [] [ text "Not authorized" ]]

                

groupManagerView = authorizator groupmanager
