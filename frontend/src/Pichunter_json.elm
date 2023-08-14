module Pichunter_json exposing (..)

import Json.Encode as Encode
import Json.Encode.Extra as Enc_Extra
import Json.Decode as Decode
import Json.Decode.Extra as Extra

import User exposing (..)
import Image exposing (..)
import GuessResult exposing (GuessResult)
import Session
import StupidTime

decodeApply : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
decodeApply value partial =
    Decode.andThen (\p -> Decode.map p value) partial

encodeMaybe valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value
        Nothing ->
            Encode.null        

encodeRegistration state =
    Encode.object
        [ ("displayname", Encode.string state.displayname)
        , ("username", Encode.string state.username)
        , ("password", Encode.string state.password)
        , ("password-again", Encode.string state.password_again)]

encodeLogin state =
    Encode.object
        [ ("username", Encode.string state.username)
        , ("password", Encode.string state.password)]

encodeUser user =
    Encode.object
        [ ("username", Encode.string user.username)
        , ("id", Encode.int user.id)
        , ("displayName", Encode.string user.displayName)
        , ("imgId", Enc_Extra.maybe Encode.string user.imgId)
        , ("activated?", Encode.bool user.activated)]
            
decodeUser =
    Decode.succeed User
        |> decodeApply (Decode.field "username" Decode.string)
        |> decodeApply (Decode.field "id" Decode.int)
        |> decodeApply (Decode.field "displayName" Decode.string)
        |> decodeApply (Decode.field "imgId" (Decode.maybe Decode.string))
        |> decodeApply (Decode.field "abilities" (Decode.list Decode.string))
        |> decodeApply (Decode.field "activated?" Decode.bool)

decodeAdministrativeUser =
    Decode.succeed User
        |> decodeApply (Decode.field "username" Decode.string)
        |> decodeApply (Decode.field "id" Decode.int)
        |> decodeApply (Decode.field "displayName" Decode.string)
        |> decodeApply (Decode.field "imgId" (Decode.maybe Decode.string))
        |> decodeApply (Decode.field "abilities" (Decode.list Decode.string))
        |> decodeApply (Decode.field "activated?" Decode.bool)

encodePermission permission =
    Encode.object 
        [ ("id", Enc_Extra.maybe Encode.int permission.id)
        , ("action", Enc_Extra.maybe Encode.string permission.action)]
           
decodePermission =
    Decode.succeed Permission
        |> decodeApply (Decode.field "id" (Decode.maybe Decode.int))
        |> decodeApply (Decode.field "action" (Decode.maybe (Decode.string)))

encodeGroup group =
    Encode.object
        [ ("id", Encode.int group.id)
        , ("name", Encode.string group.name)
        , ("description", Encode.string group.description)
        , ("users", Encode.list encodeUser group.users)
        , ("permissions", Encode.list encodePermission group.permissions)]    

decodeGroup =
    Decode.succeed Group
        |> decodeApply (Decode.field "id" Decode.int)
        |> decodeApply (Decode.field "name" Decode.string)
        |> decodeApply (Decode.field "description" Decode.string)
        |> decodeApply (Decode.field "users" (Decode.list decodeAdministrativeUser))
        |> decodeApply (Decode.field "all-users" (Decode.list decodeAdministrativeUser))
        |> decodeApply (Decode.field "permissions" (Decode.list decodePermission))
        |> decodeApply (Decode.field "all-abilities" (Decode.list decodePermission))

decodeGroupTree = Decode.list decodeGroup
encodeGroupTree = Encode.list encodeGroup

decodeImageMetadata = Decode.succeed ImageMetadata
                      |> decodeApply (Decode.field "id" Decode.string)
                      |> decodeApply (Decode.field "filename" Decode.string)
                      |> decodeApply (Decode.field "latitude" Decode.float)
                      |> decodeApply (Decode.field "longitude" Decode.float)
                      |> decodeApply (Decode.field "session-id" (Decode.maybe Decode.string))
                         
decodePicturelistResponse = Decode.list decodeImageMetadata

decodeGuessResult = Decode.succeed GuessResult
                    |> decodeApply (Decode.field "correct?" Decode.bool)


decodePictureCount =  Decode.succeed PictureCount
                   |> decodeApply (Decode.field "county_code" Decode.int)
                   |> decodeApply (Decode.field "count" Decode.int)

type alias LocationGuess =
    { pic_id: String
    , latitude: Float
    , longitude: Float}

encodeLocationGuess guess =
        Encode.object
            [ ("latitude", Encode.float guess.latitude)
            , ("longitude", Encode.float guess.longitude)
            , ("picture-id", Encode.string guess.pic_id)]

decodeGuess = Decode.succeed Session.Guess
           |> decodeApply (Decode.field "picture_id" Decode.string)
           |> decodeApply (Decode.field "correctly_guessed" Decode.bool)

decodeTimestamp =  Decode.succeed StupidTime.StupidTime
                |>  decodeApply (Decode.field "year" Decode.int)
                |> decodeApply (Decode.field "month" Decode.int)
                |> decodeApply (Decode.field "day" Decode.int)
                |> decodeApply (Decode.field "hour" Decode.int)
                |> decodeApply (Decode.field "minute" Decode.int)
                |> decodeApply (Decode.field "second" Decode.int)
                |> decodeApply (Decode.field "millisec" Decode.int)
                   
decodeSessionData =  Decode.succeed Session.Session
                  |> decodeApply (Decode.field "id" Decode.string)
                  |> decodeApply (Decode.field "started_at" decodeTimestamp)
                  |> decodeApply (Decode.field "completed_at" (Decode.maybe decodeTimestamp))
                  |> decodeApply (Decode.field "guesses" (Decode.list decodeGuess))
