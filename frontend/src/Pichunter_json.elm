module Pichunter_json exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode

import User exposing (..)

decodeApply : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
decodeApply value partial =
    Decode.andThen (\p -> Decode.map p value) partial

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

decodeUser =
    Decode.succeed User
        |> decodeApply (Decode.field "username" Decode.string)
        |> decodeApply (Decode.field "id" Decode.int)
        |> decodeApply (Decode.field "displayName" Decode.string)
        |> decodeApply (Decode.field "imgId" (Decode.maybe Decode.string))
        |> decodeApply (Decode.field "abilities" (Decode.list Decode.string))
