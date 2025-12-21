-- ExporterMakefileAdapter.hs
-- Haskell adapter to sequence and run Agda IO actions from ExporterMakefile

module Main where

import MAlonzo.Code.Examples.ExporterMakefile (main)
import Control.Monad (sequence_)

main :: IO ()
main = sequence_ main
