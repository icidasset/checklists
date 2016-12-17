module Views.Checklist exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Model.Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    case model.decodedChecklist of
        Just checklist ->
            div
                []
                [ text checklist.name
                , text (toString checklist.items)
                ]

        Nothing ->
            div
                []
                []
