module Views.Index exposing (view)

import Checklist exposing (Checklist)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Model.Types exposing (Model, Msg(..))
import Views.Utils


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
                [ Html.map HandleCreateForm (theForm model) ]
            ]
        ]



-- Intro


theIntro : Model -> Html Msg
theIntro _ =
    em
        []
        [ text "Build a checklist." ]



-- Form


theForm : Model -> Html Form.Msg
theForm model =
    Html.form
        [ onSubmit Form.Submit ]
        [ p
            []
            [ label
                [ for "name" ]
                [ text "Checklist name" ]
            , Input.textInput
                (Form.getFieldAsString "name" model.createForm)
                [ placeholder "My checklist" ]
            ]
        , p
            []
            [ label
                []
                [ text "Items" ]
            , span
                [ style [ ( "display", "block" ) ] ]
                (List.map
                    (itemView model.createForm)
                    (Form.getListIndexes "items" model.createForm)
                )
            ]
        , p
            []
            [ a
                [ onClickPreventDefault (Form.Append "items") ]
                [ text "+ Add item" ]
            ]
        , p
            []
            [ button
                [ type_ "submit" ]
                [ text "Create checklist" ]
            ]
        , Views.Utils.formErrors model.createForm
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


itemView : Form String Checklist -> Int -> Html Form.Msg
itemView leForm idx =
    let
        i =
            toString (idx)

        n =
            toString (idx + 1)
    in
        span
            []
            [ span
                [ class "form-item" ]
                [ Input.textInput
                    (Form.getFieldAsString ("items." ++ i) leForm)
                    [ placeholder ("Item " ++ n) ]
                , a
                    [ class "form-item__remove"
                    , onClickPreventDefault (Form.RemoveItem "items" idx)
                    , title "Remove item"
                    ]
                    [ text "✘" ]
                ]
            , br [] []
            ]
