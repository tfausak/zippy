module Zippy.Class.ToBytes where

import qualified Data.Bool as Bool
import qualified Data.ByteString.Builder as Builder
import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair

class ToBytes a where
  toBytes :: a -> Builder.Builder

instance ToBytes Bool where
  toBytes = Builder.word8 . Bool.bool 0x00 0x01

instance ToBytes Float where
  toBytes = Builder.floatLE

instance ToBytes Int.Int8 where
  toBytes = Builder.int8

instance ToBytes Int.Int16 where
  toBytes = Builder.int16LE

instance ToBytes Int.Int32 where
  toBytes = Builder.int32LE

instance ToBytes Json.Json where
  toBytes = Json.encode

instance ToBytes a => ToBytes (List.List a) where
  toBytes = foldMap toBytes . List.toList

instance ToBytes a => ToBytes (Option.Option a) where
  toBytes = Option.option mempty toBytes

instance (ToBytes a, ToBytes b) => ToBytes (Pair.Pair a b) where
  toBytes (Pair.Pair x y) = toBytes x <> toBytes y

instance ToBytes Word.Word8 where
  toBytes = Builder.word8

instance ToBytes Word.Word16 where
  toBytes = Builder.word16LE

instance ToBytes Word.Word32 where
  toBytes = Builder.word32LE

instance ToBytes Word.Word64 where
  toBytes = Builder.word64LE
