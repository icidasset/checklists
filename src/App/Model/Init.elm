module Model.Init exposing (withProgramFlags)

import Form
import Form.Field
import Form.Init exposing (setList)
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
        model =
            { createForm = Form.initial initialCreateFormFields Forms.Validation.createForm
            , currentPage = Routing.locationToPage location
            , decodedChecklist = Nothing
            , pathToRoot = flags.pathToRoot
            }
    in
        Model.Update.withMessage
            (SetPage model.currentPage)
            (model)



-- Private


initialCreateFormFields : List ( String, Form.Field.Field )
initialCreateFormFields =
    [ setList "items"
        [ Form.Field.string ""
        , Form.Field.string ""
        ]
    ]
