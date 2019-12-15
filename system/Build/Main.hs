module Main where

import Flow
import Protolude
import Shikensu
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Functions
import Shikensu.Utilities

import qualified Data.Aeson as Aeson (Object, Value, toJSON)
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap (fromList, singleton)
import qualified Html
import qualified Layouts.Application
import qualified Pages.Proxy
import qualified Renderers.Lucid


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main = do
    se <- sequences

    -- Execute flows
    -- & reduce to a single dictionary
    -- & write to disk
    se
        |> List.concatMap flow
        |> write "./build"



-- SEQUENCES


data Sequence
    = Images
    | Javascript
    | Pages
    | Vendor
    deriving (Eq)


sequences :: IO [ (Sequence, Dictionary) ]
sequences = lsequence
    [ ( Images,       encapsulate "icidasset-template/Images/**/*.*"  >>= Shikensu.read )
    , ( Javascript,   encapsulate "src/*.js"                          >>= Shikensu.read )
    , ( Pages,        encapsulate "src/Pages/**/*.hs"                                   )
    , ( Vendor,       encapsulate "src/Vendor/**/*.js"                >>= Shikensu.read )
    ]


flow :: (Sequence, Dictionary) -> Dictionary
flow (Pages, dict) =
    dict
        |> renameExt ".hs" ".html"
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"
        |> pathToRootForProxy
        |> copyPropsToMetadata
        |> insertMetadata (HashMap.singleton "category" "Checklists")
        |> insertMetadata (HashMap.singleton "title" "Checklists")
        |> renderContent (Renderers.Lucid.renderer <| Pages.Proxy.template)
        |> renderContent (Renderers.Lucid.renderer <| Layouts.Application.template Html.nothing)


flow (Javascript, dict) = dict
flow (Images, dict) = prefixDirname "images/" dict
flow (Vendor, dict) = prefixDirname "vendor/" dict



encapsulate :: [Char] -> IO Dictionary
encapsulate thePattern =
    Shikensu.listRelativeF "." [ thePattern ]
