port module Signals.Ports exposing (..)

-- Send


port deflate : String -> Cmd msg


port inflate : String -> Cmd msg



-- Listen


port deflateResult : (Maybe String -> msg) -> Sub msg


port inflateResult : (Maybe String -> msg) -> Sub msg
