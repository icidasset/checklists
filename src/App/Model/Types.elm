module Model.Types exposing (..)


type alias Model =
    { currentPage : Page
    , isLoading : Bool
    , pathToRoot : String
    }



-- Messages


type Msg
    = -- Navigation
      GoToIndex
    | SetPage Page



-- Routing


type Page
    = Index
    | Checklist String
    | NotFound
