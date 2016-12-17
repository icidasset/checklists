module Model.Update exposing (withMessage)

import Checklist
import Debug
import Form
import Forms.Utils exposing (..)
import Model.Types exposing (..)
import Navigation exposing (modifyUrl, newUrl)


withMessage : Msg -> Model -> ( Model, Cmd Msg )
withMessage msg model =
    case msg of
        ---------------------------------------
        -- Checklists
        ---------------------------------------
        Deflated result ->
            (!)
                model
                [ newUrl ("/checklists/" ++ result) ]

        Inflated (Just result) ->
            -- TODO: Handle decoding error
            (!)
                { model | decodedChecklist = Checklist.decode result }
                []

        Inflated Nothing ->
            -- TODO: Show error
            (!)
                { model | decodedChecklist = Nothing }
                []

        ---------------------------------------
        -- Forms
        ---------------------------------------
        HandleCreateForm formMsg ->
            let
                newModel =
                    { model
                        | createForm = Form.update formMsg model.createForm
                    }
            in
                if shouldSubmitForm formMsg newModel.createForm then
                    (!)
                        newModel
                        [ newModel.createForm
                            |> Form.getOutput
                            |> Maybe.withDefault Checklist.empty
                            |> Checklist.encode
                        ]
                else
                    (!) newModel []

        ---------------------------------------
        -- Navigation
        ---------------------------------------
        SetPage (Checklist hash) ->
            (!)
                { model | currentPage = Checklist hash, decodedChecklist = Nothing }
                [ Checklist.inflate hash ]

        SetPage page ->
            (!)
                { model | currentPage = page }
                []
