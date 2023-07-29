module Image exposing (..)

type alias ImageMetadata =
    { id: String
    , filename: String
    , latitude: Float
    , longitude: Float}

type alias PictureCount =
    { county: Int
    , count: Int }
