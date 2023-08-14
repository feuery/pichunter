module StupidTime exposing (..)

type alias StupidTime =
    { year: Int
    , month: Int
    , day: Int
    , hour: Int
    , minute: Int
    , second: Int
    , millisec:Int }

type Formatting
    = Date
    | DateTime

format formatting timestamp = 
    case formatting of
        Date -> (String.fromInt timestamp.day) ++ "." ++ (String.fromInt timestamp.month) ++ "." ++ (String.fromInt timestamp.year)
        DateTime -> (format Date timestamp) ++ " " ++ (String.fromInt timestamp.hour) ++ ":" ++ (String.fromInt timestamp.minute)
