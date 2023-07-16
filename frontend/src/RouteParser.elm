module RouteParser exposing (..)

import Url
import Url.Parser exposing (..)
import String exposing (fromInt)
-- http://localhost:3000/blog/post/edit/21
type Route
    = Home 
    | RegisterScreen
    | LoggedInHome
    | ManageUsersGroups
    | ManageMedia
    | Play
    | NotFound

routeParser =
    oneOf
        [ map Home top
        , map RegisterScreen (s "register")
        , map ManageUsersGroups (s "admin" </> (s "usersgroups"))
        , map ManageMedia (s "admin" </> (s "media"))
        , map Play (s "play")
        , map LoggedInHome (s "home")]

url_to_route url =
            Maybe.withDefault NotFound (parse routeParser url)
