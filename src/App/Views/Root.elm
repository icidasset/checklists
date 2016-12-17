module Views.Root exposing (view)

import Html exposing (Html)
import Model.Types exposing (..)
import Views.Checklist
import Views.Index
import Views.MessageScreen


view : Model -> Html Msg
view model =
    case model.currentPage of
        Checklist _ ->
            Views.Checklist.view model

        Index ->
            Views.Index.view model

        _ ->
            Views.MessageScreen.view "Page not found."
