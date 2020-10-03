module Zippy.Type.Config where

import qualified Control.Monad as Monad
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
