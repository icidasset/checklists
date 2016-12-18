module Model.Types exposing (..)

import Checklist exposing (Checklist)
import Form exposing (Form)


type alias Model =
    { createForm : Form String Checklist
    , currentPage : Page
    , decodedChecklist : Maybe Checklist
    , deflationResult : Maybe String
    , pathToRoot : String
    , redirectToChecklist : Bool
    }



-- Messages


type Msg
    = -- Checklists
      Deflated (Maybe String)
    | Inflated (Maybe String)
    | ForkCurrent
      -- Forms
    | HandleCreateForm Form.Msg
      -- Navigation
    | GoToIndex
    | SetPage Page



-- Routing


type Page
    = Index
    | Checklist (Maybe String)
    | NotFound
