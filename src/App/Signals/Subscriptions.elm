module Signals.Subscriptions exposing (list)

import Model.Types exposing (Model, Msg(..))
import Signals.Ports as Ports


list : Model -> Sub Msg
list model =
    Sub.batch
        [ Ports.deflateResult Deflated
        , Ports.inflateResult Inflated
        ]
