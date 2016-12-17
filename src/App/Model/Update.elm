module Model.Update exposing (withMessage)

import Model.Types exposing (..)
import Navigation exposing (modifyUrl, newUrl)


withMessage : Msg -> Model -> ( Model, Cmd Msg )
withMessage msg model =
    case msg of
        ---------------------------------------
        -- Navigation
        ---------------------------------------
        GoToIndex ->
            (!) model [ newUrl ("/") ]

        -- Navigation :: PRIVATE
        SetPage page ->
            (!) { model | currentPage = page } []
