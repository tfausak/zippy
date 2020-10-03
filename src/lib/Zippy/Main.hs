module Zippy.Main where

import qualified Data.Version as Version
import qualified Paths_zippy as Package
import qualified System.Environment as Environment

main :: IO ()
main = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments =
  print (name, Version.showVersion Package.version, arguments)
