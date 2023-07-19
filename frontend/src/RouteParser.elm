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
    | PlayLocationGuessing
    | NotFound

routeParser =
    oneOf
        [ map Home top
        , map RegisterScreen (s "register")
        , map ManageUsersGroups (s "admin" </> (s "usersgroups"))
        , map ManageMedia (s "admin" </> (s "media"))
        , map PlayLocationGuessing (s "play" </> (s "locationguessing"))
        , map LoggedInHome (s "home")]

url_to_route url =
            Maybe.withDefault NotFound (parse routeParser url)
