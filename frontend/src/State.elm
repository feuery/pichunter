module State exposing (..)

import File exposing (File)
import Url
import Http
import Browser

-- TODO replace with a real url handling
type Viewstate
    = Demo
    | File_upload_demo

type alias Model =
    { viewstate : Viewstate
    , world: String }

type Msg
    = Hello
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotInputFiles (List File)
    | UploadedImage (Result Http.Error ())

