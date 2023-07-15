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

loadGroupTree =
    Http.get
        { url = "/api/grouptree"
        , expect = Http.expectJson GroupTreeResult Json.decodeGroupTree}
saveGroupTree groups =
    Http.post
        { url = "/api/grouptree"
        , body = Http.jsonBody <| Json.encodeGroupTree groups
        , expect = Http.expectWhatever DummyResponse}

postPicture pictureFile = Http.post 
                          { url = "/api/pictures"
                          , body = Http.multipartBody [ Http.filePart "file" pictureFile ]
                          -- , expect = Http.expectJson UploadedImage Image.imageResponseDecoder }
                          , expect = Http.expectWhatever UploadedImage}

getPictureIds = Http.get
                { url = "/api/pictures"
                , expect = Http.expectJson GotPictureIds Json.decodePicturelistResponse}
