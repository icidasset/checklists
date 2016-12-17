module Pages.Proxy where

import Template
import qualified Data.Aeson as Aeson (encode)
import qualified Data.Text as Text (append, concat)
import qualified Data.Text.Lazy as LazyText (toStrict)
import qualified Data.Text.Lazy.Encoding as LazyText (decodeUtf8)


template :: Template
template obj _ = mconcat
  [ container_
      [ class_ "elm-container" ] ↩
      []

  , script_
      [ class_ "config", type_ "application/json" ]
      ( Aeson.encode obj )

  , script_
      [ src_ $ Text.append (obj ⚡⚡ "pathToRoot") ("application.js") ]
      ( "" )

  , script_
      [ src_ $ Text.append (obj ⚡⚡ "pathToRoot") ("elm-loader.js") ]
      ( "" )
  ]
