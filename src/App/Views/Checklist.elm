module Views.Checklist exposing (view)

import Checklist exposing (Checklist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onClickPreventDefault)
import Model.Types exposing (Model, Msg(..))
import Views.Icon
import Views.LoadingScreen
import Views.MessageScreen


view : Model -> Html Msg
view model =
    if model.isInflating then
        Views.LoadingScreen.view
    else
        case model.decodedChecklist of
            Just checklist ->
                theChecklist model checklist

            Nothing ->
                Views.MessageScreen.view "Could not decode checklist."



-- Private


theChecklist : Model -> Checklist -> Html Msg
theChecklist model checklist =
    div
        [ class "blocks" ]
        [ div
            [ class "blocks__row" ]
            [ div
                [ class "block" ]
                [ h1
                    [ style [ ( "text-align", "center" ) ] ]
                    [ text checklist.name ]
                , div
                    [ class "checklist" ]
                    (List.indexedMap
                        (\idx item ->
                            let
                                theId =
                                    "checklist__item-" ++ (toString <| idx + 1)
                            in
                                div
                                    [ class "checklist__item" ]
                                    [ input
                                        [ type_ "checkbox", name theId, value item, id theId ]
                                        []
                                    , label
                                        [ for theId ]
                                        [ span
                                            [ class "checkbox" ]
                                            [ Views.Icon.view model "i-check" ]
                                        , text item
                                        ]
                                    ]
                        )
                        (checklist.items)
                    )
                ]
            ]
        , div
            [ class "blocks__row" ]
            [ menu
                [ a
                    [ onClickPreventDefault ForkCurrent ]
                    [ text "Fork checklist" ]
                , a
                    [ href "../", onClickPreventDefault GoToIndex ]
                    [ text "Create new checklist" ]
                ]
            ]
        ]


menu : List (Html Msg) -> Html Msg
menu nodes =
    div
        [ class "block" ]
        [ div
            [ class "block__list" ]
            [ ul
                []
                (List.map
                    (\n -> li [] [ n ])
                    (nodes)
                )
            ]
        ]
