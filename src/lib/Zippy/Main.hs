module Zippy.Main where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Version as Version
import qualified Paths_zippy as Package
import qualified RocketLeague.Replay as Replay
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Config as Config
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Flag as Flag
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.Mode as Mode
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Result as Result

main :: IO ()
main = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  let
    (flags, parameters, options, problems) =
      Console.getOpt' Console.Permute descriptions arguments

  Monad.forM_ options $ \ option -> IO.hPutStrLn IO.stderr $
    "WARNING: unknown option `" <> option <> "'"

  Monad.forM_ parameters $ \ parameter -> IO.hPutStrLn IO.stderr $
    "WARNING: unexpected parameter `" <> parameter <> "'"

  Monad.forM_ problems $ IO.hPutStr IO.stderr . mappend "ERROR: "
  Monad.unless (null problems) Exit.exitFailure

  Monad.when (elem Flag.Help flags) $ do
    putStr $ Console.usageInfo (unwords [name, "version", version]) descriptions
    Exit.exitSuccess

  Monad.when (elem Flag.Version flags) $ do
    putStrLn version
    Exit.exitSuccess

  config <- Result.result die pure $ Config.fromFlags flags

  input <- case Config.input config of
    Option.None -> ByteString.getContents
    Option.Some filePath -> ByteString.readFile filePath

  output <- case Config.determineMode config of
    Mode.Decode -> do
      replay <- Result.result die pure $ Decoder.runSimple FromBytes.fromBytes input
      pure . Json.encode $ ToJson.toJson (replay :: Replay.Replay)
    Mode.Encode -> do
      json <- Result.result die pure $ Decoder.runSimple Json.decode input
      replay <- Result.result die pure $ Decoder.runSimple FromJson.fromJson json
      pure $ ToBytes.toBytes (replay :: Replay.Replay)

  case Config.output config of
    Option.None -> Builder.hPutBuilder IO.stdout output
    Option.Some filePath -> LazyByteString.writeFile filePath $ Builder.toLazyByteString output

die :: String -> IO a
die message = do
  IO.hPutStrLn IO.stderr $ "ERROR: " <> message
  Exit.exitFailure

version :: String
version = Version.showVersion Package.version

descriptions :: [Console.OptDescr Flag.Flag]
descriptions =
  [ Console.Option ['h', '?'] ["help"] (Console.NoArg Flag.Help)
    "shows this help text"
  , Console.Option ['v'] ["version"] (Console.NoArg Flag.Version)
    "shows the version number"
  , Console.Option ['i'] ["input"] (Console.ReqArg Flag.Input "FILE")
    "sets the input file (defaults to STDIN)"
  , Console.Option ['o'] ["output"] (Console.ReqArg Flag.Output "FILE")
    "sets the output file (defaults to STDOUT)"
  , Console.Option ['m'] ["mode"] (Console.ReqArg Flag.Mode "MODE")
    "sets the mode (decode or encode)"
  ]
