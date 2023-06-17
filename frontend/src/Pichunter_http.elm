module Pichunter_http exposing (..)

import Http exposing (..)
import State exposing (..)
import Pichunter_json as Json

doRegister state =
    Http.post
        { url = "/api/login/register"
        , expect = Http.expectWhatever RegistrationResult
        , body = Http.jsonBody <| Json.encodeRegistration state }
