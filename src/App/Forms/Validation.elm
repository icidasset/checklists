module Forms.Validation exposing (createForm)

import Form.Error exposing (..)
import Form.Validate exposing (..)
import Checklist exposing (Checklist)


createForm : Validation String Checklist
createForm =
    map2 Checklist
        (field "name" <| string)
        (field "items" <| itemsValidater)



-- Private


itemsValidater =
    oneOf [ string, emptyString ]
        |> list
        |> andThen nonEmptyList


nonEmptyList : List String -> Validation String (List String)
nonEmptyList list field =
    if List.isEmpty list then
        Err (Form.Error.value <| CustomError "There must be at least one item")
    else if List.any ((==) "") list then
        Err (Form.Error.value Empty)
    else
        Ok list
