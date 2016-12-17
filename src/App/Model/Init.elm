module Model.Init exposing (..)

import Model.Types exposing (Model, Msg(..))
import Navigation
import Routing


type alias ProgramFlags =
    { pathToRoot : String }


withProgramFlags : ProgramFlags -> Navigation.Location -> ( Model, Cmd Msg )
withProgramFlags flags location =
    (!)
        { currentPage = Routing.locationToPage location
        , isLoading = False
        , pathToRoot = flags.pathToRoot
        }
        []
