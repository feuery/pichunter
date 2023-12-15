module Pichunter_http exposing (..)

import Http exposing (..)
import State exposing (..)
import Image exposing (..)
import Pichunter_json as Json
import Json.Decode as D
import Json.Encode as E

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

loadHighestSessions =
    Http.get
        { url = "/api/session/highest"
        , expect = Http.expectJson GotGameSessionHighs Json.decodeHighscore}

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

session_to_getparam firstparam session_id =
    if firstparam then
        "?gamesession=" ++ session_id
    else
        "&gamesession=" ++ session_id
                   
getNextForGame county_code gamestate session_id = Http.get
                 { url = case gamestate of
                             PictureGuessingState _ _ _ _ _ _ ->
                                 let session = Maybe.withDefault "" (Maybe.map (session_to_getparam False) session_id)
                                 in
                                 "/api/next-picture/" ++ county_code ++ "?gametype=picguess" ++ session
                             _ ->
                                 "/api/next-picture/" ++ county_code                       
                 , expect = Http.expectJson GotNextPicForGame (D.maybe Json.decodeImageMetadata)}    

postLocationGuess pic_id latitude longitude gamesession =
    let guess = Json.LocationGuess pic_id latitude longitude
        session = Maybe.withDefault "" (Maybe.map (session_to_getparam True) gamesession)
    in
        Http.post
            { url = "/api/location/guess" ++ session
            , body = Http.jsonBody <| Json.encodeLocationGuess guess
            , expect = Http.expectJson UploadedGuess Json.decodeGuessResult}
                  
    
postGuessPicture gamesession pictureFile =
    let session = Maybe.withDefault "" (Maybe.map (session_to_getparam True) gamesession)
    in
    Http.post 
        { url = "/api/picture/guess" ++ session
        , body = Http.multipartBody [ Http.filePart "file" pictureFile ]
        , expect = Http.expectJson UploadedGuess Json.decodeGuessResult}

loadPictureCounts = Http.get
                    { url = "/api/pictures/count-per-county"
                    , expect = Http.expectJson GotPictureCounts (D.list Json.decodePictureCount)}
    
loadSessionData sessiontype =
    let session_url = case sessiontype of
                          Picture -> "/api/picture/session"
                          Location -> "/api/location/session"
    in 
        Http.get
            { url = session_url
            , expect = Http.expectJson (GotSessionData sessiontype) Json.decodeSessionData}

postUser new_image user fs =
    let body = case new_image of
                   Just file -> Http.multipartBody [ Http.filePart "file" file
                                                   , Http.stringPart "new_password" fs.newPassword
                                                   , Http.stringPart "old_password" fs.oldPassword
                                                   , Http.stringPart "user" (E.encode 0 (Json.encodeUser user)) ]
                   Nothing -> Http.multipartBody [ Http.stringPart "user" (E.encode 0 (Json.encodeUser user))
                                                 , Http.stringPart "new_password" fs.newPassword
                                                 , Http.stringPart "old_password" fs.oldPassword]
    in 
        Http.post
            { url = "/api/user" 
            , body = body
            , expect = Http.expectJson SavedUser Json.decodeUser}
            
getUnapprovedImgQueue =
    Http.get
        { url = "/api/pictures/unapproved"
        , expect = Http.expectJson GotUnapprovedImageQueue Json.decodePicturelistResponse}

approveImage img =
    Http.post
        { url = "/api/pictures/approve/" ++ img.id
        , body = Http.emptyBody
        , expect = Http.expectWhatever ImageApproved }

unapproveImage img =
    Http.post
        { url = "/api/pictures/unapprove/" ++ img.id
        , body = Http.emptyBody
        , expect = Http.expectWhatever ImageApproved }        
