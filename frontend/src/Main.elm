port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Url
import File exposing (File, mime)

import State exposing (..)
import Pichunter_http exposing (..)
import RouteParser exposing (..)
import HomeScreen exposing (homeScreen)
import RegistrationScreen exposing (registrationScreen)
import Header exposing (topbar)
import GroupManager exposing (groupManagerView)
import MediaManager exposing (mediaManagerView)
import UserSettings
import Game exposing (..)
import Session exposing (..)


port alert : String -> Cmd msg
port initializeAdminMaps : List (String, Float, Float) -> Cmd msg
port initGameMap : (String, Float, Float) -> Cmd msg
port checkGameFiles : String -> Cmd msg
port resetInput: String -> Cmd msg

port mapClicked : ((Float, Float, Float) -> msg) -> Sub msg
port noGpsFound : (() -> msg) -> Sub msg

viewStatePerUrl : Url.Url -> (RouteParser.Route, List (Cmd Msg))
viewStatePerUrl url =
    let route = RouteParser.url_to_route url
    in
        ( route
        , case route of
              Home -> [checkSession]
              RegisterScreen -> [checkSession]
              PlayLocationGuessing -> [ checkSession ]
              PlayPictureGuessing -> [ checkSession ]
              UserSettings -> [ checkSession ]
              ManageUsersGroups -> [ checkSession
                                   , loadGroupTree]
              ManageMedia -> [ checkSession
                             , getPictureIds
                             , getUnapprovedImgQueue]
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
              Nothing
              (case route of
                   PlayLocationGuessing -> ChoosingCounty LocationGuessing
                   PlayPictureGuessing -> ChoosingCounty PictureGuessing
                   _ -> NotPlaying)
              []
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
        , Cmd.batch cmds )
        
      
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch 
                  [ mapClicked MapClicked
                  , noGpsFound NoGpsFound]

handleSession model result =
    case result of
        Ok user ->
            ( { model | session = LoggedIn user}
            , Cmd.batch [ loadPictureCounts
                        , loadSessionData Picture
                        , loadSessionData Location
                        , loadHighestSessions])
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

add_user_to_group state group user =
    let new_group = {group | users = user :: group.users}
        new_loaded_groups = List.map (\g -> if g.id == new_group.id then
                                                new_group
                                            else g) state.loadedGroups
    in
    { state
        | selectedGroup = Just new_group
        , loadedGroups = new_loaded_groups}

drop_user_from_group state group user =
    let new_group = {group | users = List.filter ((/=) user) group.users}
        new_loaded_groups = List.map (\g -> if g.id == new_group.id then
                                                new_group
                                            else g) state.loadedGroups
    in
    { state
        | selectedGroup = Just new_group
        , loadedGroups = new_loaded_groups}
                    
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
                                                              selectedGroup.all_users) in
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
        AdminUserActivated selected_user activated ->
            case model.groupManagerState of
                Nothing -> (model, Cmd.none)
                Just state ->
                    ({ model | groupManagerState =
                           Just {state | loadedGroups =
                                     List.map (\group ->
                                                   {group |
                                                        users = List.map
                                                        (\user ->
                                                             if user.id == selected_user.id then
                                                                 {user | activated = activated}
                                                             else 
                                                                 user
                                                        ) group.users}
                                                   
                                              ) state.loadedGroups
                                , selectedUser = Just {selected_user | activated = activated}}}
                    , Cmd.none)
        AdminSetUsername selected_user name ->
            case model.groupManagerState of
                Nothing -> (model, Cmd.none)
                Just state ->
                    ({ model | groupManagerState =
                           Just {state | loadedGroups =
                                     List.map (\group ->
                                                   {group |
                                                        users = List.map
                                                        (\user ->
                                                             if user.id == selected_user.id then
                                                                 {user | displayName = name}
                                                             else 
                                                                 user
                                                        ) group.users}
                                                   
                                              ) state.loadedGroups
                                , selectedUser = Just {selected_user | displayName = name}}}
                    , Cmd.none)
        AdminUserBanned selected_user banned ->
            case model.groupManagerState of
                Nothing -> (model, Cmd.none)
                Just state ->
                    ({ model | groupManagerState =
                           Just {state | loadedGroups =
                                     List.map (\group ->
                                                   {group |
                                                        users = List.map
                                                        (\user ->
                                                             if user.id == selected_user.id then
                                                                 {user | banned = banned}
                                                             else 
                                                                 user
                                                        ) group.users}
                                                   
                                              ) state.loadedGroups
                                , selectedUser = Just {selected_user | banned = banned}}}
                    , Cmd.none)                    
        AdminUserToGroup ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            case state.selectedUser of
                                Just selectedUser ->
                                    if not (List.member selectedUser selectedGroup.users) then
                                        ({ model | groupManagerState =
                                              Just (add_user_to_group state selectedGroup selectedUser)}
                                        , Cmd.none)
                                    else
                                        ( model
                                        , alert "User is already a member of the group")
                                _ -> ( model
                                     , alert "No user selected")
                        _ -> ( model
                             , alert "No group selected")
                _ -> ( model
                     , alert "state uninited")
        AdminUserFromGroup ->
            case model.groupManagerState of
                Just state ->
                    case state.selectedGroup of
                        Just selectedGroup ->
                            case state.selectedUser of
                                Just selectedUser ->
                                    ( {model | groupManagerState =
                                           Just (drop_user_from_group state selectedGroup selectedUser)}
                                    , Cmd.none)
                                _ -> ( model
                                     , alert "No user selected")
                        _ -> ( model
                             , alert "No group selected")
                _ -> ( model
                     , alert "state uninited")
        SaveGroupManagerState ->
            case model.groupManagerState of
                Just state ->            
                    ( model
                    , saveGroupTree state.loadedGroups)
                _ -> ( model
                     , alert "group mgr state uninited")
        GotInputFiles files ->
            if List.all (\file -> String.startsWith "image" (mime file)) files then
                ( model
                , Cmd.batch
                    (List.concat
                         [ [ resetInput "mediamanager_input"
                           , checkGameFiles "mediamanager_input"]
                         , (List.map (\file -> postPicture file) files)]))
            else
                ( model
                , alert ("Expected images, got " ++ (String.join ", " (List.map mime files))))
        UploadedImage result ->
            ( model
            , getPictureIds)
        GotUnapprovedImageQueue result ->
            case result of
                Ok imgs ->
                    ({ model | mediaManagerState = Just (case model.mediaManagerState of
                                                             Just mmState ->
                                                                 {mmState | unapproved_queue = imgs}
                                                             Nothing ->
                                                                 MediaManagerState [] imgs)}
                    , Cmd.none)
                Err error ->
                    ( model
                    , alert ("fetching unapprovedqueue failed " ++ (Debug.toString error)))
        GotPictureIds result ->
            case result of
                Ok list_of_ids ->
                    ( { model
                          | mediaManagerState =
                            Just (case model.mediaManagerState of
                                      Just mmstate -> { mmstate | known_metadata = list_of_ids}
                                      Nothing -> MediaManagerState list_of_ids [])}
                          
                    , initializeAdminMaps (List.map (\meta ->
                                                         ( MediaManager.map_id_to_element_id meta.id
                                                         , meta.latitude
                                                         , meta.longitude)) list_of_ids))
                Err error ->
                    ( model
                    , alert ("Error in gotpictureids: " ++ (Debug.toString error)))
        RemovePicture metadata ->
            ( model
            , removePicture metadata.id)
        RemovalResult result ->
            case result of
                Ok success -> (model, getPictureIds)
                Err error -> (model, alert (Debug.toString error))
        GotNextPicForGame result ->
            case result of
                Ok maybe_meta ->
                    case maybe_meta of
                        Just meta ->
                            case model.gameState of
                                LocationGuessingState _ score tries county ->
                                    ( { model
                                          | gameState = LocationGuessingState (Just meta) score tries county
                                          , sessionId = meta.session_id}
                                    , initGameMap ( MediaManager.map_id_to_element_id meta.id
                                                  , meta.latitude
                                                  , meta.longitude))
                                PictureGuessingState _ score tries county allow_usage _ ->
                                    ( { model
                                          | gameState = PictureGuessingState (Just meta) score tries county allow_usage []
                                          , sessionId = meta.session_id}
                                    , initGameMap ( MediaManager.map_id_to_element_id meta.id
                                                  , meta.latitude
                                                  , meta.longitude))
                                _ -> ( model
                                     , alert ("invalid state " ++ (Debug.toString model.gameState)))
                        Nothing ->
                            ( model
                            , alert ("NextPic returned " ++ (Debug.toString result) ++ ". You probably won the game?"))
                Err error ->
                    (model, alert (Debug.toString error))
        MapClicked (distance, latitude, longitude) ->
            case model.gameState of
                LocationGuessingState meta score tries county ->
                    case meta of
                        Just pic ->
                            ( model
                            , postLocationGuess pic.id latitude longitude model.sessionId)
                        Nothing -> ( model
                                   , alert "Wtf pic is nil?")
                _ -> ( model
                     , alert ("State " ++ (Debug.toString model.gameState) ++ " is invalid"))
        ChoseCounty game_type county_code ->
            case (String.toInt county_code) of
                Just county ->
                    let state = case game_type of
                                    LocationGuessing -> LocationGuessingState Nothing 0 0 county
                                    PictureGuessing -> PictureGuessingState Nothing 0 0 county False []
                                    
                    in
                        ( { model | gameState = state }
                        , getNextForGame county_code state model.sessionId)
                Nothing ->
                    ( model
                    , alert ("Can't parse county code " ++ county_code ))
        SetAllowForUsage allowed ->
            case model.gameState of
                PictureGuessingState meta score tries county _  files->
                    ( { model | gameState = PictureGuessingState meta score tries county allowed files}
                    , Cmd.none)
                _ -> ( model
                     , Cmd.none)
        GotGameFiles files ->
            case model.gameState of
                PictureGuessingState meta score tries county allow_usage _ ->
                    ( { model | gameState = PictureGuessingState meta score tries county allow_usage files}
                    , checkGameFiles "game_file")
                _ -> ( model, Cmd.none)
        NoGpsFound _ ->
            ( case model.gameState of
                  PictureGuessingState meta score tries county allow_usage _ ->
                      { model | gameState = PictureGuessingState meta score tries county allow_usage []}
                  _ -> model
            , alert "Image you selected doesn't seem to contain gps coordinates")
        SubmitGuess ->
            case model.gameState of
                PictureGuessingState meta score tries county allow_usage files ->                
                    ( model
                    , Cmd.batch (List.map (postGuessPicture model.sessionId) files))
                _ ->
                    ( model
                    , alert "How have you ended up in SubmitGuess with gameState that's != PictureGuessingState?")
        UploadedGuess result ->
            case result of
                Ok guessresult ->
                    case model.gameState of
                        PictureGuessingState _ _ _ county_code _ _ ->
                            if guessresult.correct then
                                ( model
                                , Cmd.batch [ alert "Oikein :D"
                                            , getNextForGame (String.fromInt county_code) model.gameState model.sessionId])
                            else
                                ( model
                                , alert "Väärin :D")
                        LocationGuessingState pic score tries county ->
                            if guessresult.correct then
                                ( { model
                                        | gameState = LocationGuessingState pic (score + 1) (tries + 1) county }
                                , getNextForGame (String.fromInt county) model.gameState model.sessionId)
                            else
                                ( { model
                                        | gameState = LocationGuessingState pic score (tries + 1) county }
                                , alert "Wrong!")
                                        
                        _ -> ( model, alert ("You should not be able to fire a UploadedGuess from the state of " ++ (Debug.toString model.gameState)))
                Err err ->
                    ( model
                    , alert ("Error: " ++ (Debug.toString err)))
        GotPictureCounts result ->
            case result of
                Ok counts ->
                    ( { model | imageCounts = counts}
                    , Cmd.none)
                Err err ->
                    ( model
                    , alert ("Error: " ++ (Debug.toString err)))
        GotSessionData sessiontype result ->
            case result of
                Ok sessiondata ->
                    case sessiontype of
                        Picture ->
                            ({ model
                                 | picGameSession = Just sessiondata}
                            , Cmd.none)
                        Location -> 
                            ({ model
                                 | locationGameSession = Just sessiondata}
                            , Cmd.none)
                Err err ->
                    case err of
                        Http.BadStatus status ->
                            if status == 404 then
                                ( model
                                , Cmd.none)
                            else 
                                ( model
                                , alert ("Didn't get session data due to: " ++ (Debug.toString err)))
                        _ -> ( model
                             , alert ("Didn't get session data due to: " ++ (Debug.toString err)))
        ChangeUserField formstate title value ->
            let new_model = { model
                                | usersettingsform = 
                                  Just (case title of
                                           "Login username" -> { formstate
                                                                   | username = value}
                                           "Display name" -> { formstate | displayname  = value}
                                           "Old password" -> { formstate | oldPassword = value}
                                           "Password" -> { formstate | newPassword = value}
                                           _ -> formstate)}
            in ( new_model, Cmd.none)
        SaveLoggedInUser user formstate ->
            (model, postUser formstate.newImage
                 {user
                     | username = formstate.username
                     , displayName = formstate.displayname} formstate)
        SavedUser result -> (model, checkSession)
        GotUserFile files ->
            case model.session of
                LoggedIn user ->
                    let settings = UserSettings.defaultFormState user model.usersettingsform
                    in
                        ( { model |
                                usersettingsform = Just { settings | newImage = List.head files}}
                        , Cmd.none)
                _ -> ( model
                     , Cmd.none)
        GotGameSessionHighs result ->
            case result of
                Ok sessiondata ->
                    ( { model
                          | highestSessionData = Just sessiondata}
                    , Cmd.none)
                Err err ->
                    case err of
                        Http.BadStatus status ->
                            if status == 404 then
                                ( model
                                , Cmd.none)
                            else 
                                ( model
                                , alert ("(inner) Didn't get high score data due to: " ++ (Debug.toString err)))
                        _ -> ( model
                             , alert ("(outer) Didn't get high score data due to: " ++ (Debug.toString err)))
        ApproveImage img approved ->
            ( model
            , if approved then
                  approveImage img
              else
                  unapproveImage img)
        ImageApproved result ->
            case result of
                Err err ->
                    ( model
                    , alert ("Error approving img: " ++ Debug.toString err))
                Ok _ ->
                    ( model
                    , Cmd.batch [ getPictureIds
                                , getUnapprovedImgQueue])

                                        
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

highscore_to_view: Maybe HighscoreRow -> String
highscore_to_view maybe_row =
    case maybe_row of
        Just row -> (String.fromInt row.correct_guesses) ++ "/" ++ (String.fromInt row.all_guesses)
        Nothing -> "No high score. Have you played this?"

user_meta user =                    
    [ img [ class "picture profile-picture"
          , src ("/api/avatar/" ++ (Maybe.withDefault "" user.imgId))] []
    , p [ class "displayname"] [ text user.displayName ]
    , p [ class "username"] [ text user.username ]]

session_summary highscores =
    [ h3 [] [ text "Your highest scores"]
    , ol []
        [ li [] [ h4 [] [text "Location: "]
                , div [] [text (highscore_to_view highscores.location)]]
        , li [] [ h4 [] [text "Picture: "]
                , div [] [text (highscore_to_view highscores.picture)]]]]
    
view : Model -> Browser.Document Msg
view model =
    { title = "Hello pichunter!"
    , body = [ topbar model.session model.loginState
             , div [ class "body_container" ]
                 [ div [class "sidebar"
                       , id "left_sidebar"]
                       (List.append 
                            (case model.session of
                              LoggedIn user ->
                                  user_meta user
                              LoggedOut -> [])
                            (case model.highestSessionData of
                              Just highscores -> 
                                  session_summary highscores 
                              Nothing -> []))
                                   
                 , article [ ]
                     (case model.route of
                          Home -> homeScreen model
                          RegisterScreen -> registrationScreen model.registrationFormState
                          PlayPictureGuessing -> gameview model.session model.gameState model.imageCounts
                          PlayLocationGuessing -> gameview model.session model.gameState model.imageCounts
                          UserSettings ->
                              UserSettings.view model.session model.usersettingsform
                          NotFound -> [ div [] [ text "Not found!" ] ]
                          ManageUsersGroups -> groupManagerView model.groupManagerState model.session
                          ManageMedia ->
                              case model.mediaManagerState of
                                  Just state -> mediaManagerView state model.session
                                  Nothing -> [ div [] [ text "Media manager hasn't loaded"]])]]}

