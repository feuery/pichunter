module Pichunter_http exposing (..)

import Http exposing (..)
import State exposing (..)
import Image exposing (..)
import Pichunter_json as Json
import Json.Decode as D 

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

removePicture id = Http.request
                   { url = "/api/pictures/" ++ id
                   , method = "DELETE"
                   , headers = []
                   , body = Http.emptyBody
                   , timeout = Nothing
                   , tracker = Nothing
                   , expect = Http.expectJson RemovalResult D.bool}
                   
getNextForGame county_code gamestate = Http.get
                 { url = case gamestate of
                             PictureGuessingState _ _ _ _ _ _ ->
                                 "/api/next-picture/" ++ county_code ++ "?gametype=picguess"
                             _ ->
                                 "/api/next-picture/" ++ county_code                       
                 , expect = Http.expectJson GotNextPicForGame (D.maybe Json.decodeImageMetadata)}

postGuessPicture pictureFile = Http.post 
                               { url = "/api/guess-picture"
                               , body = Http.multipartBody [ Http.filePart "file" pictureFile ]
                               , expect = Http.expectJson UploadedGuess Json.decodeGuessResult}

loadPictureCounts = Http.get
                    { url = "/api/pictures/count-per-county"
                    , expect = Http.expectJson GotPictureCounts (D.list Json.decodePictureCount)}
