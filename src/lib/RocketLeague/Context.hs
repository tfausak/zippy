module RocketLeague.Context (Context(..), new) where

import qualified Control.Exception as Exception
import qualified Exception.MissingMaxChannels as MissingMaxChannels
import qualified Exception.InvalidMaxChannels as InvalidMaxChannels
import qualified RocketLeague.Header as Header
import qualified RocketLeague.Dict as Dict
import qualified RocketLeague.Property as Property
import qualified RocketLeague.I32 as I32
import qualified RocketLeague.Property.Int as IntProperty
import qualified RocketLeague.Content as Content
import qualified RocketLeague.Version as Version

data Context = Context
  { maxChannels :: Int
  , numBits :: Int
  , version :: Version.Version
  } deriving (Eq, Show)

new :: Header.Header -> Content.Content frames -> Either Exception.SomeException Context
new header _content = do
  maxChannels <- case Dict.lookup "MaxChannels" $ Header.properties header of
    Just property -> case property of
      Property.Int x -> Right . I32.toInt $ IntProperty.toI32 x
      _ -> Left . Exception.toException $ InvalidMaxChannels.InvalidMaxChannels property
    Nothing -> Left . Exception.toException $ MissingMaxChannels.MissingMaxChannels
  Right Context
    { maxChannels
    , numBits = floor (logBase 2 $ fromIntegral maxChannels :: Double)
    , version = Header.version header
    }
