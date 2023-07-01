port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http
import Url

import State exposing (..)
import Pichunter_http exposing (..)
import RouteParser exposing (..)
import HomeScreen exposing (homeScreen)
import RegistrationScreen exposing (registrationScreen)
import Header exposing (topbar)
import GroupManager exposing (groupManagerView)

port alert : String -> Cmd msg

viewStatePerUrl : Url.Url -> (RouteParser.Route, List (Cmd Msg))
viewStatePerUrl url =
    let route = RouteParser.url_to_route url
    in
        ( route
        , case route of
              Home -> [checkSession]
              RegisterScreen -> [checkSession]
              LoggedInHome -> [checkSession]
              ManageUsersGroups -> [ checkSession
                                   , loadGroupTree
                                   ]
              NotFound -> [checkSession])
        
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

init _ url key =
    let (route, cmds) = viewStatePerUrl url
    in 
        ( Model route key (case route of
                               RegisterScreen -> Just (RegistrationForm "" "" "" "")
                               _ -> Nothing)
              (LoginForm "" "")
              LoggedOut
              Nothing
        , Cmd.batch cmds )
        
      
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch 
                  [ ]

handleSession model result =
    case result of
        Ok user ->
            ( { model | session = LoggedIn user}
            , Cmd.none)
        Err error ->
            case error of
                Http.BadStatus status ->
                    if status == 401 then
                        ( model
                        , Cmd.none)
                    else
                        ( model
                        , Cmd.none)
                _ ->
                    ( model
                    , alert (Debug.toString error))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UrlChanged url ->
            init Nothing url model.key
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)
        RegistrationDisplayname name ->
            ({ model | registrationFormState =
                   case model.registrationFormState of
                       Just state ->
                           Just { state | displayname = name}
                       Nothing -> Nothing}
            , Cmd.none)
        RegistrationUsername name ->
            ({ model | registrationFormState =
                   case model.registrationFormState of
                       Just state ->
                           Just { state | username = name}
                       Nothing -> Nothing}
            , Cmd.none)
        RegistrationPassword nth password ->
            ({ model | registrationFormState =
                   case model.registrationFormState of
                       Just state ->
                           case nth of
                               First -> Just { state | password = password}
                               Second -> Just { state | password_again = password}

                       Nothing -> Nothing}
            , Cmd.none)
        SendRegistration formState -> (model, doRegister formState)
        DummyResponse _ -> (model, Cmd.none)
        LoginUsername name ->
            let loginstate = model.loginState in
            ({ model | loginState =
                   { loginstate | username = name}}
            , Cmd.none)
        LoginPassword password ->
            let loginstate = model.loginState in
            ({ model | loginState =
                   { loginstate | password = password }}
            , Cmd.none)
        Login state ->
            ( { model | loginState = (LoginForm "" "")}
            , login state )
        Logout ->
            ( model
            , logout)
        LogoutResult res ->
            case res of
                Ok _ -> 
                    ({ model | session = LoggedOut }
                    , Cmd.none)
                Err error ->
                    ( model
                    , alert (Debug.toString error))
        LoginResult result ->
            handleSession model result
        SessionResult result ->
            handleSession model result
        GroupTreeResult result ->
            case result of
                Ok groups ->
                    ( { model
                          | groupManagerState = Just (GroupManagerState Nothing Nothing Nothing groups)}
                            
                    , Cmd.none)
                Err error ->
                    ( model
                    , alert ("Parsing grouptreeresult failed due to " ++ (Debug.toString error)))
        AdminGroupSelected groupid ->
            case model.groupManagerState of
                Just state ->
                    let groups = state.loadedGroups 
                        selectedgroup = List.head (List.filter
                                                       (\group -> (String.fromInt group.id) == groupid)
                                                       groups) in 
                    ( { model | groupManagerState =
                            Just { state
                                     | selectedGroup = selectedgroup
                                     , selectedUser = Nothing}}
                    , Cmd.none)
                _ -> ( model, alert "GroupManager uninitialized")

        AdminUserSelected userid ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            let selectedUser = List.head (List.filter
                                                              (\user -> (String.fromInt user.id) == userid)
                                                              selectedGroup.users) in
                            ( {model | groupManagerState =
                                   Just { state | selectedUser = selectedUser} }
                            , Cmd.none)
                        Nothing -> ( model, alert "no group selected")
                Nothing -> ( model, alert "group state is nil")
        AdminSelectExistingAbility permission_id ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            case List.head (List.filter
                                                (\permission ->
                                                     case permission.id of
                                                         Just id ->
                                                             (String.fromInt id) == permission_id
                                                         _ -> False)
                                                selectedGroup.permissions) of
                                Just permission -> 
                                    ( { model | groupManagerState =
                                            Just { state | selectedPermission = Just permission}}
                                    , Cmd.none)
                                Nothing -> ( model
                                           , alert ("no permission " ++ permission_id ++ " selected"))
                        Nothing -> ( model
                                   , alert "no group selected")
                Nothing -> ( model
                           , alert "no state initialized")
        AdminSelectNonExistingAbility permission_id ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            case List.head (List.filter
                                                (\permission ->
                                                     case permission.id of
                                                         Just id ->
                                                             (String.fromInt id) == permission_id
                                                         _ -> False)
                                                selectedGroup.all_abilities) of
                                Just permission -> 
                                    ( { model | groupManagerState =
                                            Just { state | selectedPermission = Just permission}}
                                    , Cmd.none)
                                Nothing -> ( model
                                           , alert ("no permission " ++ permission_id ++ " selected"))
                        Nothing -> ( model
                                   , alert "no group selected")
                Nothing -> ( model
                           , alert "no state initialized")
        AdminDisallow ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            case state.selectedPermission of
                                Just selectedPermission ->
                                    let new_state = disallow_permission state selectedGroup selectedPermission
                                    in
                                    ( { model
                                          | groupManagerState =
                                            Just new_state}
                                    , Cmd.none)
                                _ -> (model, alert "no permission selected")
                        _ -> (model, alert "no group selected")
                _ -> (model, alert "groupmanager uninitialized")
        AdminAllow ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            case state.selectedPermission of
                                Just selectedPermission ->
                                    let new_state = allow_permission state selectedGroup selectedPermission
                                    in
                                    ( { model
                                          | groupManagerState =
                                            Just new_state}
                                    , Cmd.none)
                                _ -> (model, alert "no permission selected")
                        _ -> (model, alert "no group selected")
                _ -> (model, alert "groupmanager uninitialized")
                                    
disallow_permission state old_group permission 
    = let group = { old_group | permissions =
                        (  old_group.permissions
                        |> List.filter ((/=) permission)) } in
      { state
          | selectedGroup = Just group
          , loadedGroups = (  state.loadedGroups
                           |> List.map (\g -> if g.id == group.id then
                                                  group
                                              else
                                                  g))}
allow_permission state old_group permission 
    = let group = { old_group | permissions = permission :: old_group.permissions} in
      { state
          | selectedGroup = Just group
          , loadedGroups = (  state.loadedGroups
                           |> List.map (\g -> if g.id == group.id then
                                                  group
                                              else
                                                  g))}          
                     
view : Model -> Browser.Document Msg
view model =
    { title = "Hello pichunter!"
    , body = (topbar model.session model.loginState)
             ::
             (case model.route of
                 Home -> homeScreen
                 RegisterScreen -> registrationScreen model.registrationFormState
                 ManageUsersGroups -> groupManagerView model.groupManagerState model.session
                 _ -> [div [] [ text "Hello World!" ]])}
