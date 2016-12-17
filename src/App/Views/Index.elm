module Views.Index exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div
        [ class "blocks" ]
        [ div
            [ class "blocks__row" ]
            [ div
                [ class "block" ]
                [ theIntro model ]
            ]
        , div
            [ class "blocks__row" ]
            [ div
                [ class "block" ]
                [ theForm model ]
            ]
        ]



-- Intro


theIntro : Model -> Html Msg
theIntro _ =
    em
        []
        [ text "Build a checklist." ]



-- Form


theForm : Model -> Html Msg
theForm _ =
    Html.form
        []
        [ p
            []
            [ label
                []
                [ text "Checklist name" ]
            , input
                [ type_ "text", placeholder "My checklist" ]
                []
            ]
        , p
            []
            [ label
                []
                [ text "Items" ]
            , input
                [ type_ "text", placeholder "1st item on my checklist" ]
                []
            , input
                [ type_ "text", placeholder "2nd item on my checklist" ]
                []
            ]
        , p
            []
            [ a
                []
                [ text "+ Add item" ]
            ]
        , p
            []
            [ button
                [ type_ "submit" ]
                [ text "Create checklist" ]
            ]
        , p
            []
            [ small
                []
                [ em
                    []
                    [ text "Unique ID length, "
                    , strong [] [ text "800" ]
                    , text " / 1900."
                    ]
                ]
            ]
        ]
