module Forms.Init exposing (..)

import Form.Field
import Form.Init exposing (setList)


initialCreateFormFields : List ( String, Form.Field.Field )
initialCreateFormFields =
    [ setList "items"
        [ Form.Field.string ""
        , Form.Field.string ""
        ]
    ]
