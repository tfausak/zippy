module Zippy.Config (Config(..), fromFlags) where

import qualified Data.List as List
import qualified Zippy.Flag as Flag

data Config = Config
  { help :: Bool
  , input :: FilePath
  , output :: FilePath
  , version :: Bool
  } deriving (Eq, Show)

fromFlags :: [Flag.Flag] -> Config
fromFlags = List.foldl' applyFlag initial

applyFlag :: Config -> Flag.Flag -> Config
applyFlag config flag = case flag of
  Flag.Help -> config { help = True }
  Flag.Input x -> config { input = x }
  Flag.Output x -> config { output = x }
  Flag.Version -> config { version = True }

initial :: Config
initial = Config
  { help = False
  , input = "-"
  , output = "-"
  , version = False
  }
