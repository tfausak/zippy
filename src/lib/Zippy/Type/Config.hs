module Zippy.Type.Config where

import qualified Control.Monad as Monad
import qualified System.FilePath as FilePath
import qualified Zippy.Type.Flag as Flag
import qualified Zippy.Type.Mode as Mode

data Config = Config
  { input :: Maybe FilePath
  , mode :: Maybe Mode.Mode
  , output :: Maybe FilePath
  } deriving (Eq, Show)

initial :: Config
initial = Config
  { input = Nothing
  , mode = Nothing
  , output = Nothing
  }

fromFlags :: [Flag.Flag] -> Either String Config
fromFlags = Monad.foldM withFlag initial

withFlag :: Config -> Flag.Flag -> Either String Config
withFlag config flag = case flag of
  Flag.Help -> Right config
  Flag.Input filePath -> Right config { input = Just filePath }
  Flag.Mode string -> do
    theMode <- Mode.fromString string
    Right config { mode = Just theMode }
  Flag.Output filePath -> Right config { output = Just filePath }
  Flag.Version -> Right config

determineMode :: Config -> Mode.Mode
determineMode config = case mode config of
  Just theMode -> theMode
  Nothing -> case fmap FilePath.takeExtension (input config) of
    Just ".json" -> Mode.Encode
    Just ".replay" -> Mode.Decode
    _ -> case fmap FilePath.takeExtension (output config) of
      Just ".replay" -> Mode.Encode
      _ -> Mode.Decode
