module RocketLeague.Replay (Replay(..), fromBytes) where

import qualified Zippy.ByteGet as ByteGet

data Replay header content = Replay
  { header :: header
  , content :: content
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet header -> ByteGet.ByteGet content -> ByteGet.ByteGet (Replay header content)
fromBytes getHeader getContent = ByteGet.label "Replay" $ do
  header <- ByteGet.label "header" getHeader
  content <- ByteGet.label "content" getContent
  pure Replay { header, content }
