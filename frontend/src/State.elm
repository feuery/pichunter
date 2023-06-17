module State exposing (..)

import File exposing (File)
import Url
import Http
import Browser
import RouteParser exposing (..)
import Browser.Navigation as Nav

type alias RegistrationForm =
    { displayname: String
    , username: String
    , password: String
    , password_again: String}

type alias Model =
    { route: Route
    , key: Nav.Key
    , registrationFormState: Maybe RegistrationForm
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
    | RegistrationResult (Result Http.Error ())
