module Views.Index exposing (view)

import Checklist exposing (Checklist)
import Constants
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Model.Types exposing (Model, Msg(..))
import Views.Icon
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
                [ div
                    [ class "block__text" ]
                    [ Html.map HandleCreateForm (theForm model) ]
                ]
            , div
                [ class "block block--filler", attribute "hide-lt" "small" ]
                [ div
                    [ class "block--filler__inner" ]
                    [ Views.Icon.view model "i-check" ]
                ]
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
    let
        errors =
            Form.getErrors model.createForm

        deflationLength =
            if List.isEmpty errors then
                model.deflationResult
                    |> Maybe.withDefault ""
                    |> String.length
            else
                0
    in
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
            , Views.Utils.formErrors model.createForm False
            , if deflationLength > Constants.maxDeflationHashLength then
                p
                    [ class "form__error" ]
                    [ text "You have too much text, see counter below." ]
              else
                text ""
            , p
                []
                [ small
                    []
                    [ em
                        []
                        [ text "Unique ID length, "
                        , strong
                            []
                            [ deflationLength
                                |> toString
                                |> text
                            ]
                        , text (" / " ++ toString Constants.maxDeflationHashLength ++ ".")
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

        field =
            Form.getFieldAsString ("items." ++ i) leForm
    in
        span
            []
            [ span
                [ class "form-item" ]
                [ Input.textInput
                    field
                    [ placeholder ("Item " ++ n), value (Maybe.withDefault "" field.value) ]
                , a
                    [ class "form-item__remove"
                    , onClickPreventDefault (Form.RemoveItem "items" idx)
                    , title "Remove item"
                    ]
                    [ text "✘" ]
                ]
            ]
