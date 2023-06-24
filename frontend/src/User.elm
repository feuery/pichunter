module User exposing (..)

type alias User =
    { username: String
    , id: Int
    , displayName: String
    , imgId: Maybe String}
