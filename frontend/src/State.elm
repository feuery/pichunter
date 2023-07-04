module State exposing (..)

import File exposing (File)
import Url
import Http
import Browser
import RouteParser exposing (..)
import Browser.Navigation as Nav
import User exposing (..)

type alias RegistrationForm =
    { displayname: String
    , username: String
    , password: String
    , password_again: String}

type Session = LoggedIn User
    | LoggedOut
    
type alias LoginForm =
    { username: String
    , password: String }

type alias GroupManagerState =
    { selectedGroup: Maybe Group
    , selectedUser: Maybe User
    , selectedPermission: Maybe Permission
    , loadedGroups: List Group}
    
type alias Model =
    { route: Route
    , key: Nav.Key
    , registrationFormState: Maybe RegistrationForm
    , loginState: LoginForm
    , session: Session
    , groupManagerState: Maybe GroupManagerState
    }

type Nth
    = First
    | Second

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | RegistrationDisplayname String
    | RegistrationUsername String
    | RegistrationPassword Nth String
    | SendRegistration RegistrationForm
    | DummyResponse (Result Http.Error ())
    | LoginResult (Result Http.Error User)
    | LoginUsername String
    | LoginPassword String
    | Login LoginForm
    | Logout
    | LogoutResult (Result Http.Error ())
    | SessionResult (Result Http.Error User)
    | GroupTreeResult (Result Http.Error (List Group))
    -- strings here are ids, because you cant set an 'a as elm's html option's value
    | AdminGroupSelected String
    | AdminUserSelected String
    | AdminSelectExistingAbility String
    | AdminDisallow
    | AdminAllow
    | AdminSelectNonExistingAbility String
    | AdminUserToGroup
    | AdminUserFromGroup
    | SaveGroupManagerState
