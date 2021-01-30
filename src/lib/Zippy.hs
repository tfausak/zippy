module Zippy (defaultMain, mainWith) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Paths_zippy as Package
import qualified RocketLeague.Content as Content
import qualified RocketLeague.Frames as Frames
import qualified RocketLeague.Header as Header
import qualified RocketLeague.Replay as Replay
import qualified RocketLeague.Section as Section
import qualified RocketLeague.Stream as Stream
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Zippy.ByteGet as ByteGet
import qualified Zippy.Config as Config
import qualified Zippy.Flag as Flag

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  config <- getConfig name arguments
  let
    getInput = case Config.input config of
      "-" -> ByteString.getContents
      file -> ByteString.readFile file
  input <- getInput
  replay <- runByteGet (Replay.fromBytes Section.fromBytes Section.fromBytes) input
  header <- runByteGet Header.fromBytes . Section.value $ Replay.header replay
  content <- runByteGet (Content.fromBytes Stream.fromBytes) . Section.value $ Replay.content replay
  frames <- runByteGet (Frames.fromBytes header content) . Stream.toByteString $ Content.frames content
  let
    putOutput = case Config.output config of
      "-" -> ByteString.putStr
      file -> ByteString.writeFile file
  putOutput
    . Text.encodeUtf8
    . Text.pack
    $ show Replay.Replay { Replay.header = header, Replay.content = content { Content.frames = frames } }

runByteGet :: ByteGet.ByteGet a -> ByteString.ByteString -> IO a
runByteGet g s1 = case ByteGet.run g s1 of
  Left (ls, e) -> do
    IO.hPutStrLn IO.stderr . List.intercalate ": "
      $ ls <> [Exception.displayException e]
    Exit.exitFailure
  Right (_s2, x) -> do
    -- TODO: Make sure s2 is empty.
    pure x

getConfig :: String -> [String] -> IO Config.Config
getConfig name arguments = do
  let
    (flags, parameters, options, problems) =
      Console.getOpt' Console.Permute optionDescriptions arguments
  Monad.forM_ parameters $ \ parameter ->
    IO.hPutStrLn IO.stderr $ "WARNING: unexpected parameter `" <> parameter <> "'"
  Monad.forM_ options $ \ option ->
    IO.hPutStrLn IO.stderr $ "WARNING: unknown option `" <> option <> "'"
  Monad.forM_ problems $ \ problem ->
    IO.hPutStr IO.stderr $ "ERROR: " <> problem
  Monad.unless (null problems) Exit.exitFailure
  let config = Config.fromFlags flags
  Monad.when (Config.help config) $ do
    putStr $ Console.usageInfo name optionDescriptions
    Exit.exitSuccess
  Monad.when (Config.version config) $ do
    putStrLn $ Version.showVersion Package.version
    Exit.exitSuccess
  pure config

optionDescriptions :: [Console.OptDescr Flag.Flag]
optionDescriptions =
  [ Console.Option ['h', '?'] ["help"] (Console.NoArg Flag.Help) ""
  , Console.Option ['v'] ["version"] (Console.NoArg Flag.Version) ""
  , Console.Option ['i'] ["input"] (Console.ReqArg Flag.Input "FILE") ""
  , Console.Option ['o'] ["output"] (Console.ReqArg Flag.Output "FILE") ""
  ]
