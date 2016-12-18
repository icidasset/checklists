module Views.Utils exposing (formErrors)

import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import Model.Types exposing (Model, Msg)
import String.Extra exposing (humanize)


formErrors : Form String o -> Bool -> Html Form.Msg
formErrors form bypass =
    if Form.isSubmitted form || bypass then
        p
            [ class "form__error" ]
            [ case List.head (Form.getErrors form) of
                Just ( label, err ) ->
                    case err of
                        CustomError e ->
                            text (e ++ ".")

                        Empty ->
                            text (properLabel label ++ " cannot be empty.")

                        InvalidString ->
                            text (properLabel label ++ " cannot be empty.")

                        _ ->
                            text ("The field `" ++ properLabel label ++ "` is invalid.")

                _ ->
                    text ""
            ]
    else
        text ""



-- Private


properLabel : String -> String
properLabel label =
    humanize label
