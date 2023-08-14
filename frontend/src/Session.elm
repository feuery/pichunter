module Session exposing (..)

import StupidTime 

type alias Guess =
    { picture_id: String
    , correctly_guessed: Bool}
                         

type alias Session =
    { id: String
    , started_at: StupidTime.StupidTime
    , completed_at: Maybe StupidTime.StupidTime
    , guesses: List Guess}
