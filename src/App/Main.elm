module Main exposing (..)

import Model.Init
import Model.Update
import Navigation
import Routing
import Signals.Subscriptions
import Views.Root


main =
    Navigation.programWithFlags Routing.locationToMessage
        { init = Model.Init.withProgramFlags
        , view = Views.Root.view
        , update = Model.Update.withMessage
        , subscriptions = Signals.Subscriptions.list
        }
