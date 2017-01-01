module Pages.Proxy where

import Template
import qualified Components.LoadingScreenScript
import qualified Data.Aeson as Aeson (encode)


template :: Template
template obj _ = mconcat
    [ container_
        [ class_ "elm-container" ] ↩
        []

    , Components.LoadingScreenScript.template ".elm-container" obj

    , script_
        [ class_ "config", type_ "application/json" ]
        ( Aeson.encode obj )

    , relativeScript_ (obj ⚡⚡ "pathToRoot") ("vendor/pako.min.js")
    , relativeScript_ (obj ⚡⚡ "pathToRoot") ("application.js")
    , relativeScript_ (obj ⚡⚡ "pathToRoot") ("elm-loader.js")
    ]
