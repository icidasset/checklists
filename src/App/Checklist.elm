module Checklist exposing (..)

import Json.Decode
import Json.Encode
import Signals.Ports as Ports



-- Types


type alias Checklist =
    { name : String
    , items : List ( String, Bool )
    }



-- Instances


empty : Checklist
empty =
    { name = ""
    , items = []
    }



-- Encoding & Decoding


encode : Checklist -> Cmd msg
encode checklist =
    checklist
        |> toJsonValue
        |> Json.Encode.encode 0
        |> Ports.deflate


toJsonValue : Checklist -> Json.Encode.Value
toJsonValue checklist =
    Json.Encode.object
        [ ( "name", Json.Encode.string checklist.name )
        , ( "items", Json.Encode.list (List.map encodeItem checklist.items) )
        ]


encodeItem : ( String, Bool ) -> Json.Encode.Value
encodeItem ( value, isChecked ) =
    Json.Encode.list
        [ Json.Encode.string value
        , Json.Encode.bool isChecked
        ]


inflate : String -> Cmd msg
inflate str =
    Ports.inflate str


decode : String -> Maybe Checklist
decode str =
    Json.Decode.decodeString decoder str
        |> Result.toMaybe


decoder : Json.Decode.Decoder Checklist
decoder =
    Json.Decode.map2 Checklist
        (Json.Decode.field "name" <| Json.Decode.string)
        (Json.Decode.field "items" <| Json.Decode.list itemDecoder)


itemDecoder : Json.Decode.Decoder ( String, Bool )
itemDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map2
            (,)
            (Json.Decode.index 0 Json.Decode.string)
            (Json.Decode.index 1 Json.Decode.bool)
        , Json.Decode.map
            (\v -> ( v, False ))
            Json.Decode.string
        ]
