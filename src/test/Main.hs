import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Word as Word
import qualified GHC.Clock as Clock
import qualified System.Directory as Directory
import qualified System.Mem as Mem
import qualified Text.Printf as Printf
import qualified Zippy

main :: IO ()
main = do
  (allocations, (duration, _)) <- withAllocations . withDuration $
    mapM_ test replays

  Printf.printf
    "total allocations: %s bytes, duration: %s nanoseconds\n"
    (format allocations)
    (format duration)

test :: String -> IO ()
test replay = do
  let input = "replays/" <> replay <> ".replay"
  size <- Directory.getFileSize input
  Printf.printf "%s input %s bytes\n" replay (format size)

  let json = "output/" <> replay <> ".json"
  do
    (allocations, duration) <- zippy input json
    Printf.printf
      "  decode allocations: %s bytes, duration: %s nanoseconds\n"
      (format allocations)
      (format duration)

  let output = "output/" <> replay <> ".replay"
  do
    (allocations, duration) <- zippy json output
    Printf.printf
      "  encode allocations: %s bytes, duration: %s nanoseconds\n"
      (format allocations)
      (format duration)

format :: Show a => a -> String
format = reverse . List.intercalate "," . chunksOf 3 . reverse . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = case splitAt n xs of
  ([], _) -> []
  (ys, zs) -> ys : chunksOf n zs

zippy :: FilePath -> FilePath -> IO (Int.Int64, Word.Word64)
zippy input output = do
  (allocations, (duration, _)) <- withAllocations . withDuration $
    Zippy.mainWith "zippy" ["--input", input, "--output", output]
  pure (allocations, duration)

withAllocations :: IO a -> IO (Int.Int64, a)
withAllocations action = do
  before <- Mem.getAllocationCounter
  result <- action
  after <- Mem.getAllocationCounter
  pure (before - after, result)

withDuration :: IO a -> IO (Word.Word64, a)
withDuration action = do
  before <- Clock.getMonotonicTimeNSec
  result <- action
  after <- Clock.getMonotonicTimeNSec
  pure (after - before, result)

replays :: [String]
replays =
  [ "0008"
  , "000b"
  , "0416"
  , "07e9"
  , "0ad2"
  , "1205"
  , "160c"
  , "16d5"
  , "18d6"
  , "1a12"
  , "1ae4"
  , "1bc2"
  , "1d1d"
  , "1ec9"
  , "1ef9"
  , "1f37"
  , "2114"
  , "21a8"
  , "2266"
  , "22ba"
  , "27b6"
  , "29f5"
  , "2cfe"
  , "3381"
  , "372d"
  , "383e"
  , "387f"
  , "3abd"
  , "3ea1"
  , "4126"
  , "419a"
  , "42f0"
  , "42f2"
  , "4bc3"
  , "504e"
  , "520e"
  , "524f"
  , "52aa"
  , "540d"
  , "551c"
  , "5a06"
  , "6210"
  , "6320"
  , "6688"
  , "6b0d"
  , "6d1b"
  , "6f7c"
  , "7083"
  , "7109"
  , "7256"
  , "75ce"
  , "7bf6"
  , "89cb"
  , "8ae5"
  , "92a6"
  , "946f"
  , "9704"
  , "98e5"
  , "9a2c"
  , "9e35"
  , "9eaa"
  , "a09e"
  , "a128"
  , "a184"
  , "a1c0"
  , "a52f"
  , "a558"
  , "a671"
  , "a676"
  , "a7f0"
  , "a9df"
  , "aa70"
  , "afb1"
  , "b9f9"
  , "c14f"
  , "c23b"
  , "c837"
  , "cc4c"
  , "d044"
  , "d1d5"
  , "d236"
  , "d428"
  , "d44c"
  , "d52e"
  , "d7fb"
  , "d818"
  , "db70"
  , "dcab"
  , "dcb3"
  , "de56"
  , "e80d"
  , "e978"
  , "eae3"
  , "eae8"
  , "edbb"
  , "f299"
  , "f7b9"
  , "f811"
  , "fdc7"
  , "ffb7"
  ]
