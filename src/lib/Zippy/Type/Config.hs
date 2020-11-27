module Zippy.Type.Config where

import qualified Control.Monad as Monad
import qualified System.FilePath as FilePath
import qualified Zippy.Type.Flag as Flag
import qualified Zippy.Type.Mode as Mode
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Result as Result

data Config = Config
  { input :: Option.Option FilePath
  , mode :: Option.Option Mode.Mode
  , output :: Option.Option FilePath
  } deriving (Eq, Show)

initial :: Config
initial = Config
  { input = Option.None
  , mode = Option.None
  , output = Option.None
  }

fromFlags :: [Flag.Flag] -> Result.Result String Config
fromFlags = Monad.foldM withFlag initial

withFlag :: Config -> Flag.Flag -> Result.Result String Config
withFlag config flag = case flag of
  Flag.Help -> Result.Pass config
  Flag.Input filePath -> Result.Pass config { input = Option.Some filePath }
  Flag.Mode string -> case Mode.fromString string of
    Result.Fail problem -> Result.Fail problem
    Result.Pass mode -> Result.Pass config { mode = Option.Some mode }
  Flag.Output filePath -> Result.Pass config { output = Option.Some filePath }
  Flag.Version -> Result.Pass config

determineMode :: Config -> Mode.Mode
determineMode config = case mode config of
  Option.Some mode -> mode
  Option.None -> case fmap FilePath.takeExtension $ input config of
    Option.Some ".json" -> Mode.Encode
    Option.Some ".replay" -> Mode.Decode
    _ -> case fmap FilePath.takeExtension $ output config of
      Option.Some ".json" -> Mode.Decode
      Option.Some ".replay" -> Mode.Encode
      _ -> Mode.Decode
