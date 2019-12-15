module Views.Index exposing (view)

import Checklist exposing (Checklist)
import Chunky exposing (..)
import Constants
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Model.Types exposing (Model, Msg(..))
import Views.Block as Block
import Views.Icon as Icon
import Views.Utils


view : Model -> Html Msg
view model =
    raw
        [ Block.row
            []
            [ Block.node
                [ "italic" ]
                [ theIntro model ]
            ]
        , Block.row
            []
            [ Block.node
                []
                [ Html.map HandleCreateForm (theForm model)
                ]
            , Block.node
                [ "bg-gray-100"
                , "hidden"
                , "items-center"
                , "justify-center"
                , "text-gray-300"
                , "text-4xl"

                --
                , "md:flex"

                --
                , "dark:bg-gray-800"
                , "dark:text-gray-600"
                ]
                [ Icon.view "i-check" model.pathToRoot ]
            ]
        ]



-- Intro


theIntro : Model -> Html Msg
theIntro _ =
    text "Make a checklist."



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
        [ chunk
            []
            [ label
                [ for "name" ]
                [ text "Checklist name" ]
            , Input.textInput
                (Form.getFieldAsString "name" model.createForm)
                [ placeholder "My checklist" ]
            ]

        --
        , chunk
            [ "mt-6" ]
            [ label
                []
                [ text "Items" ]
            , raw
                (List.map
                    (itemView model.pathToRoot model.createForm)
                    (Form.getListIndexes "items" model.createForm)
                )
            ]

        --
        , p
            []
            [ slab
                a
                [ onClickPreventDefault (Form.Append "items") ]
                [ "cursor-pointer" ]
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
                [ text "Your items are too long, see counter below." ]

          else
            text ""
        , p
            []
            [ slab
                small
                []
                [ "italic" ]
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


itemView : String -> Form String Checklist -> Int -> Html Form.Msg
itemView pathToRoot leForm idx =
    let
        ( i, n ) =
            ( toString idx
            , toString (idx + 1)
            )

        field =
            Form.getFieldAsString ("items." ++ i) leForm
    in
    chunk
        [ "mt-px"
        , "relative"
        ]
        [ Input.textInput
            field
            [ placeholder ("Item " ++ n)
            , value (Maybe.withDefault "" field.value)
            ]
        , slab
            a
            [ onClickPreventDefault (Form.RemoveItem "items" idx)
            , title "Remove item"
            ]
            [ "absolute"
            , "cursor-pointer"
            , "right-0"
            , "text-gray-500"
            , "text-lg"
            , "top-1/2"
            , "-translate-y-1/2"

            --
            , "dark:text-gray-900"
            ]
            [ Icon.view "i-circle-with-cross" pathToRoot
            ]
        ]
