module Main where

import Data.Text (Text)
import Flow
import Shikensu
import Shikensu.Functions
import Shikensu.Metadata
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Types
import Utilities

import qualified Data.Aeson as Aeson (Object, Value, toJSON)
import qualified Data.HashMap.Strict as HashMap (fromList, singleton)
import qualified Data.Text as Text (pack)

import qualified Layouts.Application
import qualified Pages.Proxy
import qualified Renderers.Lucid


-- | (• ◡•)| (❍ᴥ❍ʋ)


type Dependencies = Aeson.Object


main :: IO Dictionary
main =
    let
        sequencer = dependencies
            |> fmap flow
            |> fmap concatMap
    in
        sequencer
            >>= (<&>) sequences
            >>= write "./build"



-- Sequences


sequences :: IO [(String, Dictionary)]
sequences = lsequence
    [ ( "pages",  process ["src/Pages/**/*.hs"]                                   )
    , ( "js",     process ["src/*.js"]                          >>= Shikensu.read )
    , ( "vendor", process ["src/Vendor/**/*.js"]                >>= Shikensu.read )
    , ( "images", process ["icidasset-template/images/**/*.*"]  >>= Shikensu.read )
    ]


flow :: Dependencies -> (String, Dictionary) -> Dictionary
flow deps ("pages", dict) =
    dict
        |> renameExt ".hs" ".html"
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"
        |> pathToRootForProxy
        |> copyPropsToMetadata
        |> insertMetadata deps
        |> insertMetadata (HashMap.singleton "category" "Checklists")
        |> insertMetadata (HashMap.singleton "title" "Checklists")
        |> renderContent (Renderers.Lucid.catalogRenderer pagesCatalog)
        |> renderContent (Renderers.Lucid.renderer Layouts.Application.template)


flow _ ("js", dict) = dict
flow _ ("vendor", dict) = prefixDirname "vendor/" dict
flow _ ("images", dict) = prefixDirname "images/" dict


pagesCatalog :: TemplateCatalog
pagesCatalog =
    HashMap.fromList
        [ ("200", Pages.Proxy.template)
        , ("index", Pages.Proxy.template)
        ]



-- Additional IO
--   (Next to the sequences)


dependencies :: IO Dependencies
dependencies =
    []
        |> lsequence
        |> fmap (fmap $ \(a, b) -> (Text.pack a, b))
        |> fmap (HashMap.fromList)
