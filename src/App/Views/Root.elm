module Views.Root exposing (view)

import Html exposing (Html)
import Model.Types exposing (..)
import Views.Index
import Views.MessageScreen


view : Model -> Html Msg
view model =
    case model.currentPage of
        Index ->
            Views.Index.view model

        _ ->
            Views.MessageScreen.view "Page not found."
