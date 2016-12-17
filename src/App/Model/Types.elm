module Model.Types exposing (..)

import Checklist exposing (Checklist)
import Form exposing (Form)


type alias Model =
    { createForm : Form String Checklist
    , currentPage : Page
    , decodedChecklist : Maybe Checklist
    , pathToRoot : String
    }



-- Messages


type Msg
    = -- Checklists
      Deflated String
    | Inflated (Maybe String)
      -- Forms
    | HandleCreateForm Form.Msg
      -- Navigation
    | SetPage Page



-- Routing


type Page
    = Index
    | Checklist String
    | NotFound
