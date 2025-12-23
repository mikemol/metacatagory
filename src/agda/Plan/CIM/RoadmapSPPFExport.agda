{-# OPTIONS --without-K #-}

-- | RoadmapSPPFExport: runnable edge module that reads tasks.json and writes SPPF JSON
-- Implemented entirely via GHC FFI to avoid cubical infectivity for compilation.

module Plan.CIM.RoadmapSPPFExport where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.IO using (IO)

{-# FOREIGN GHC
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L

data Item = Item
  { iId       :: T.Text
  , iTitle    :: T.Text
  , iStatus   :: T.Text
  , iCategory :: T.Text
  , iSource   :: T.Text
  , iFiles    :: [T.Text]
  , iTags     :: [T.Text]
  } deriving (Show)

instance Aeson.FromJSON Item where
  parseJSON = Aeson.withObject "Item" $ \v -> Item
    <$> v Aeson..: "id"
    <*> v Aeson..: "title"
    <*> v Aeson..: "status"
    <*> v Aeson..: "category"
    <*> v Aeson..: "source"
    <*> v Aeson..: "files"
    <*> v Aeson..: "tags"

renderStr :: T.Text -> T.Text
renderStr s = T.concat [T.pack "\"", s, T.pack "\""]

renderListStr :: [T.Text] -> T.Text
renderListStr [] = T.pack "[]"
renderListStr (x:xs) = T.concat [T.pack "[", renderStr x, T.pack (concatMap (\y -> "," ++ T.unpack (renderStr y)) xs), T.pack "]"]

renderItem :: Item -> T.Text
renderItem it = T.concat
  [ T.pack "{"
  , T.pack "\"id\":", renderStr (iId it), T.pack ","
  , T.pack "\"title\":", renderStr (iTitle it), T.pack ","
  , T.pack "\"status\":", renderStr (iStatus it), T.pack ","
  , T.pack "\"category\":", renderStr (iCategory it), T.pack ","
  , T.pack "\"source\":", renderStr (iSource it), T.pack ","
  , T.pack "\"files\":", renderListStr (iFiles it), T.pack ","
  , T.pack "\"tags\":", renderListStr (iTags it), T.pack ","
  , T.pack "\"parents\":[]"
  , T.pack "}"
  ]

renderItems :: [Item] -> T.Text
renderItems [] = T.pack "[]"
renderItems (x:xs) = T.concat [T.pack "[", renderItem x, T.pack (concatMap (\y -> "," ++ T.unpack (renderItem y)) xs), T.pack "]"]

renderGraph :: [Item] -> T.Text
renderGraph items = T.concat [T.pack "{\"nodes\":", renderItems items, T.pack "}"]

exportSPPF :: T.Text -> T.Text -> IO ()
exportSPPF inPath outPath = do
  bs <- BSL.readFile (T.unpack inPath)
  case Aeson.decode bs of
    Nothing    -> fail ("Failed to decode tasks.json: " <> T.unpack inPath)
    Just items -> TIO.writeFile (T.unpack outPath) (renderGraph items)
#-}

postulate
  exportAdapter : String → String → IO ⊤

{-# COMPILE GHC exportAdapter = exportSPPF #-}

main : IO ⊤
main = exportAdapter ".github/roadmap/tasks.json" "build/gp_roadmap_sppf.json"
