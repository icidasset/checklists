module Signals.Subscriptions exposing (list)

import Model.Types exposing (Model, Msg(..))


list : Model -> Sub Msg
list model =
    Sub.none
