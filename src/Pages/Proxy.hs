module Pages.Proxy where

import Common
import Chunky
import Flow
import Html
import Html.Attributes (className, id, typ)
import Protolude
import Shikensu.Utilities

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Components.LoadingScreenScript
import qualified Data.Aeson as Aeson (encode)
import qualified Html.Custom as Html
import qualified Shikensu


-- 🍯


template :: Shikensu.Metadata -> Html -> Html
template obj _ = mconcat
    [ container
        [ chunk
          [ "elm-container" ]
          []
        ]

    , Components.LoadingScreenScript.template ".elm-container" obj

    , script
        [ id "config", typ "application/json" ]
        [ obj
            |> Aeson.encode
            |> LazyText.decodeUtf8
            |> LazyText.toStrict
            |> Html.unencoded
        ]

    , Html.relativeScript (obj !~> "pathToRoot") ("vendor/pako.min.js")
    , Html.relativeScript (obj !~> "pathToRoot") ("application.js")
    , Html.relativeScript (obj !~> "pathToRoot") ("index.js")
    ]
