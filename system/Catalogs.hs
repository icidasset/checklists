module Catalogs where

import Types

import qualified Data.HashMap.Strict as HashMap (fromList)
import qualified Pages.Proxy


pages :: TemplateCatalog
pages =
  HashMap.fromList
    [ ("200", Pages.Proxy.template)
    ]
