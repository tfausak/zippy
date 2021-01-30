module RocketLeague.Frames (Frames, fromArray, toArray, fromBytes) where

import qualified Data.Array as Array
import qualified Exception.InvalidNumFrames as InvalidNumFrames
import qualified RocketLeague.Content as Content
import qualified RocketLeague.Context as Context
import qualified RocketLeague.Dict as Dict
import qualified RocketLeague.Frame as Frame
import qualified RocketLeague.Header as Header
import qualified RocketLeague.I32 as I32
import qualified RocketLeague.Property as Property
import qualified RocketLeague.Property.Int as IntProperty
import qualified Zippy.BitGet as BitGet
import qualified Zippy.BitString as BitString
import qualified Zippy.ByteGet as ByteGet
import qualified Zippy.Get as Get

newtype Frames
  = Frames (Array.Array Int Frame.Frame)
  deriving (Eq, Show)

fromArray :: Array.Array Int Frame.Frame -> Frames
fromArray = Frames

toArray :: Frames -> Array.Array Int Frame.Frame
toArray (Frames x) = x

fromBytes :: Header.Header -> Content.Content frames -> ByteGet.ByteGet Frames
fromBytes header content = ByteGet.label "Frames" $ do
  numFrames <- case Dict.lookup "NumFrames" $ Header.properties header of
    Just property -> case property of
      Property.Int x -> pure . min 1 {- TODO -} . I32.toInt $ IntProperty.toI32 x
      _ -> ByteGet.throw $ InvalidNumFrames.InvalidNumFrames property
    Nothing -> pure 0
  context <- either ByteGet.throw pure $ Context.new header content
  byteString <- Get.get
  Get.embed (fromBitsWith context (numFrames - 1) 0 []) $ BitString.fromByteString byteString

fromBitsWith :: Context.Context -> Int -> Int -> [(Int, Frame.Frame)] -> BitGet.BitGet Frames
fromBitsWith context limit index frames = if index > limit
  then pure . fromArray $ Array.array (0, limit) frames
  else do
    frame <- ByteGet.label (show index) $ Frame.fromBits context
    fromBitsWith context limit (index + 1) $ (index, frame) : frames
