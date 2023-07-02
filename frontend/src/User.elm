module User exposing (..)

type alias User =
    { username: String
    , id: Int
    , displayName: String
    , imgId: Maybe String
    , abilities: List String}

type alias Permission =
    { id: Maybe Int
    , action: Maybe String }

type alias Group =
    { id: Int
    , name: String
    , description: String
    , users: List User
    , all_users: List User
    , permissions: List Permission
    , all_abilities: List Permission}
