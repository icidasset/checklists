module Model.Types exposing (..)

import Checklist exposing (Checklist)
import Form exposing (Form)


type alias Model =
    { createForm : Form String Checklist
    , currentPage : Page
    , decodedChecklist : Maybe Checklist
    , deflationResult : Maybe String
    , isInflating : Bool
    , pathToRoot : String
    , redirectToChecklist : Bool
    }



-- Messages


type Msg
    = -- Checklists
      Deflated (Maybe String)
    | Inflated (Maybe String)
    | ForkCurrent
    | ToggleItem String Bool
      -- Forms
    | HandleCreateForm Form.Msg
      -- Navigation
    | GoToIndex
    | NewChecklist
    | SetPage Page



-- Routing


type Page
    = Index
    | Checklist (Maybe String)
    | ErrorScreen String
