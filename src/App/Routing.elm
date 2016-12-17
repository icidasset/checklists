module Routing exposing (locationToMessage, locationToPage)

import Model.Types exposing (..)
import Navigation
import UrlParser exposing (..)


{-| Parse the location and return a `Msg`.
-}
locationToMessage : Navigation.Location -> Msg
locationToMessage location =
    location
        |> locationToPage
        |> SetPage


{-| Parse the location and return a `Page`.
-}
locationToPage : Navigation.Location -> Page
locationToPage location =
    location
        |> UrlParser.parsePath route
        |> Maybe.withDefault NotFound



-- Private


route : Parser (Page -> a) a
route =
    oneOf
        [ map Checklist (s "checklist" </> string)
        , map Index top
        ]
