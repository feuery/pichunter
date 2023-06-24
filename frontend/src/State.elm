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

type alias Model =
    { route: Route
    , key: Nav.Key
    , registrationFormState: Maybe RegistrationForm
    , loginState: LoginForm
    , session: Session
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
