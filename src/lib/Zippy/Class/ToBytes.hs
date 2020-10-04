module Zippy.Class.ToBytes where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Zippy.Type.Option as Option

class ToBytes a where
  toBytes :: a -> Builder.Builder

instance ToBytes Int.Int8 where
  toBytes = Builder.int8

instance ToBytes Int.Int16 where
  toBytes = Builder.int16LE

instance ToBytes Int.Int32 where
  toBytes = Builder.int32LE

instance ToBytes a => ToBytes (Option.Option a) where
  toBytes = Option.option mempty toBytes

instance ToBytes Word.Word8 where
  toBytes = Builder.word8

instance ToBytes Word.Word16 where
  toBytes = Builder.word16LE

instance ToBytes Word.Word32 where
  toBytes = Builder.word32LE
