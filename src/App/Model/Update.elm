module Model.Update exposing (withMessage)

import Checklist
import Constants
import Debug
import Form exposing (Form)
import Form.Field
import Form.Init
import Forms.Init
import Forms.Utils exposing (..)
import Model.Types exposing (..)
import Navigation exposing (modifyUrl, newUrl)
import Signals.Ports as Ports


withMessage : Msg -> Model -> ( Model, Cmd Msg )
withMessage msg model =
    case msg of
        ---------------------------------------
        -- Checklists
        ---------------------------------------
        Deflated (Just result) ->
            (!)
                { model | deflationResult = Just result }
                [ if
                    model.redirectToChecklist
                        && String.length result
                        <= Constants.maxDeflationHashLength
                  then
                    newUrl ("/checklist?id=" ++ result)

                  else
                    Cmd.none
                ]

        Deflated Nothing ->
            if model.redirectToChecklist then
                (!)
                    { model | deflationResult = Nothing }
                    [ newUrl "/error/deflation" ]

            else
                (!)
                    model
                    []

        --
        -- Inflate
        --
        Inflated (Just result) ->
            let
                checklist =
                    Checklist.decode result
            in
            (!)
                { model | decodedChecklist = checklist, isInflating = False }
                [ case checklist of
                    Just c ->
                        Ports.setDocumentTitle (c.name ++ " / Checklists – I.A.")

                    Nothing ->
                        Cmd.none
                ]

        Inflated Nothing ->
            (!)
                { model | decodedChecklist = Nothing, isInflating = False }
                []

        --
        -- Fork
        --
        ForkCurrent ->
            let
                checklist =
                    Maybe.withDefault Checklist.empty model.decodedChecklist

                formFields =
                    formFieldsForChecklist checklist

                updatedForm =
                    Form.update (Form.Reset formFields) model.createForm

                newModel =
                    { model | createForm = updatedForm, redirectToChecklist = False }
            in
            (!)
                newModel
                [ newUrl "/"
                , encodingChecklistCmd newModel.createForm
                ]

        --
        -- Toggle
        --
        ToggleItem item bool ->
            case model.decodedChecklist of
                Just checklist ->
                    let
                        newChecklist =
                            { checklist
                                | items =
                                    List.map
                                        (\( i, b ) ->
                                            if i == item then
                                                ( i, bool )

                                            else
                                                ( i, b )
                                        )
                                        checklist.items
                            }
                    in
                    (!)
                        { model
                            | decodedChecklist = Just newChecklist
                            , redirectToChecklist = True
                        }
                        [ Checklist.encode newChecklist ]

                Nothing ->
                    (!) model []

        ---------------------------------------
        -- Forms
        ---------------------------------------
        HandleCreateForm formMsg ->
            let
                updatedForm =
                    Form.update formMsg model.createForm

                newModel =
                    { model | createForm = updatedForm }
            in
            if shouldSubmitForm formMsg updatedForm then
                (!)
                    { newModel | redirectToChecklist = True }
                    [ encodingChecklistCmd updatedForm ]

            else
                (!)
                    { newModel | redirectToChecklist = False }
                    [ encodingChecklistCmd updatedForm ]

        ---------------------------------------
        -- Navigation
        ---------------------------------------
        GoToIndex ->
            (!) model [ newUrl "/" ]

        NewChecklist ->
            let
                updatedForm =
                    Form.update
                        (Form.Reset Forms.Init.initialCreateFormFields)
                        model.createForm
            in
            (!)
                { model | createForm = updatedForm }
                [ newUrl "/" ]

        SetPage (Checklist (Just hash)) ->
            (!)
                { model | currentPage = Checklist (Just hash), isInflating = True }
                [ Checklist.inflate hash ]

        SetPage page ->
            (!)
                { model | currentPage = page }
                []



-- Helpers


formToChecklist : Form String Checklist.Checklist -> Checklist.Checklist
formToChecklist theForm =
    theForm
        |> Form.getOutput
        |> Maybe.withDefault Checklist.empty


encodingChecklistCmd : Form String Checklist.Checklist -> Cmd Msg
encodingChecklistCmd theForm =
    theForm
        |> formToChecklist
        |> Checklist.encode


formFieldsForChecklist : Checklist.Checklist -> List ( String, Form.Field.Field )
formFieldsForChecklist checklist =
    let
        items =
            List.map (Tuple.first >> Form.Field.string) checklist.items
    in
    [ Form.Init.setString "name" checklist.name
    , Form.Init.setList "items" items
    ]
