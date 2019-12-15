module Views.Checklist exposing (view)

import Checklist exposing (Checklist)
import Chunky exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Html.Events.Extra exposing (onClickPreventDefault)
import Model.Types exposing (Model, Msg(..))
import Views.Block as Block
import Views.Icon as Icon
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
    raw
        [ Block.node
            []
            [ slab
                h1
                []
                [ "mb-16"
                , "normal-case"
                , "text-center"
                ]
                [ text checklist.name ]

            --
            , chunk
                []
                (List.indexedMap
                    (\idx ( item, isChecked ) ->
                        let
                            theId =
                                "checklist__item-" ++ (toString <| idx + 1)
                        in
                        chunk
                            []
                            [ slab
                                input
                                [ type_ "checkbox"
                                , name theId
                                , value item
                                , checked isChecked
                                , id theId
                                , onCheck (ToggleItem item)
                                ]
                                [ "hidden" ]
                                []
                            , slab
                                label
                                [ for theId ]
                                [ "flex"
                                , "font-normal"
                                , "items-center"
                                , "max-w-lg"
                                , "mb-4"
                                , "mx-auto"
                                , "text-base"
                                ]
                                [ chunk
                                    [ "flex-shrink-0"
                                    , "mr-4"
                                    , "relative"
                                    , "rounded"
                                    ]
                                    [ chunk
                                        [ "absolute"
                                        , "left-1/2"
                                        , "text-base"
                                        , "top-1/2"
                                        , "translate-centered"

                                        --
                                        , "dark:text-base07"
                                        ]
                                        [ Icon.view "i-check" model.pathToRoot ]
                                    ]
                                , text item
                                ]
                            ]
                    )
                    checklist.items
                )
            ]
        , Block.row
            []
            [ menu
                [ a
                    [ onClickPreventDefault ForkCurrent ]
                    [ text "Fork checklist" ]
                , a
                    [ href "../", onClickPreventDefault NewChecklist ]
                    [ text "Create new checklist" ]
                ]
            ]
        ]


menu : List (Html Msg) -> Html Msg
menu nodes =
    Block.node
        []
        [ ul
            []
            (List.map
                (\n -> li [] [ n ])
                nodes
            )
        ]
