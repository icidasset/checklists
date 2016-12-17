module Forms.Utils exposing (..)

import Form exposing (Form)
import Model.Types exposing (Model)


shouldSubmitForm : Form.Msg -> Form e o -> Bool
shouldSubmitForm formMsg form =
    (formMsg == Form.Submit) && (formIsValid form)


formIsValid : Form e o -> Bool
formIsValid form =
    List.isEmpty (Form.getErrors form)
