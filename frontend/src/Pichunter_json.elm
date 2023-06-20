module Pichunter_json exposing (..)

import Json.Encode as Json

encodeRegistration state =
    Json.object
        [ ("displayname", Json.string state.displayname)
        , ("username", Json.string state.username)
        , ("password", Json.string state.password)
        , ("password-again", Json.string state.password_again)] 
