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
        |> Maybe.withDefault (ErrorScreen "Page not found.")



-- Private


route : Parser (Page -> a) a
route =
    oneOf
        [ map Checklist (s "checklist" <?> stringParam "id")
        , map Index top
          -- Errors
        , map (ErrorScreen "Could not encode checklist.") (s "error" </> s "deflation")
        ]
