module Main where

import Data.Text (Text)
import Catalogs
import Flow
import Layouts.Application
import Renderers.Lucid
import Shikensu
import Shikensu.Functions
import Shikensu.Metadata
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Utilities

import qualified Data.Aeson as Aeson (Object, Value, toJSON)
import qualified Data.HashMap.Strict as HashMap (fromList, singleton)
import qualified Data.Text as Text (pack)


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
  , ( "images", process ["icidasset-template/images/**/*.*"]  >>= Shikensu.read )
  ]


flow :: Dependencies -> (String, Dictionary) -> Dictionary
flow deps ("pages", dict) =
  dict
    |> renameExt ".hs" ".html"
    |> rename "Proxy.html" "200.html"
    |> lowerCaseBasename
    |> pathToRootForProxy
    |> copyPropsToMetadata
    |> insertMetadata deps
    |> insertMetadata (HashMap.singleton "category" "Checklists")
    |> renderContent (Renderers.Lucid.catalogRenderer Catalogs.pages)
    |> renderContent (Renderers.Lucid.renderer Layouts.Application.template)
    |> clone "200.html" "index.html"


flow _ ("js", dict) = dict
flow _ ("images", dict) = prefixDirname "images/" dict



-- Additional IO
--   (Next to the sequences)


dependencies :: IO Dependencies
dependencies =
  lsequence
    []

  |> fmap (fmap $ \(a, b) -> (Text.pack a, b))
  |> fmap (HashMap.fromList)
