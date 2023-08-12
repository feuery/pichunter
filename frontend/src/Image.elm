module Image exposing (..)

type alias ImageMetadata =
    { id: String
    , filename: String
    , latitude: Float
    , longitude: Float
    -- this is relevant only in the locationguessing mode...
    , session_id: Maybe String}

type alias PictureCount =
    { county: Int
    , count: Int }
