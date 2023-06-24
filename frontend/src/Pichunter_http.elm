module Pichunter_http exposing (..)

import Http exposing (..)
import State exposing (..)
import Pichunter_json as Json

doRegister state =
    Http.post
        { url = "/api/login/register"
        , expect = Http.expectWhatever DummyResponse
        , body = Http.jsonBody <| Json.encodeRegistration state }

login state =
    Http.post
        { url = "/api/login"
        , expect = Http.expectJson LoginResult Json.decodeUser
        , body = Http.jsonBody <| Json.encodeLogin state }

checkSession =
    Http.get
        { url = "/api/session"
        , expect = Http.expectJson SessionResult Json.decodeUser}

logout =
    Http.get
        { url = "/api/logout"
        , expect = Http.expectWhatever LogoutResult}
