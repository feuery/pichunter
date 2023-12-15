module Image exposing (..)

import User exposing (..)

type alias ImageMetadata =
    { id: String
    , filename: String
    , latitude: Float
    , longitude: Float
    -- this is relevant only in the locationguessing mode...
    , session_id: Maybe String
    , approver: Maybe User}

type alias PictureCount =
    { county: Int
    , count: Int }
