module Model.Init exposing (withProgramFlags)

import Form
import Forms.Init
import Forms.Validation
import Model.Types exposing (Model, Msg(..))
import Model.Update
import Navigation
import Routing


type alias ProgramFlags =
    { pathToRoot : String }


withProgramFlags : ProgramFlags -> Navigation.Location -> ( Model, Cmd Msg )
withProgramFlags flags location =
    let
        initialCreateForm =
            Form.initial
                Forms.Init.initialCreateFormFields
                Forms.Validation.createForm

        model =
            { createForm = initialCreateForm
            , currentPage = Routing.locationToPage location
            , decodedChecklist = Nothing
            , deflationResult = Nothing
            , isInflating = False
            , pathToRoot = flags.pathToRoot
            , redirectToChecklist = False
            }
    in
        Model.Update.withMessage
            (SetPage model.currentPage)
            (model)
