module Zippy.Main where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Version as Version
import qualified Paths_zippy as Package
import qualified System.Environment as Environment
import qualified System.Console.GetOpt as Console
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Zippy.Type.Config as Config
import qualified Zippy.Type.Flag as Flag

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

  Monad.forM_ options (\ option -> IO.hPutStrLn IO.stderr
    ("WARNING: unknown option `" <> option <> "'"))

  Monad.forM_ parameters (\ parameter -> IO.hPutStrLn IO.stderr
    ("WARNING: unexpected parameter `" <> parameter <> "'"))

  Monad.unless (null problems) (do
    mapM_ (IO.hPutStr IO.stderr . mappend "ERROR: ") problems
    Exit.exitFailure)

  Monad.when (elem Flag.Help flags) (do
    putStr (Console.usageInfo (unwords [name, "version", version]) descriptions)
    Exit.exitSuccess)

  Monad.when (elem Flag.Version flags) (do
    putStrLn version
    Exit.exitSuccess)

  config <- case Config.fromFlags flags of
    Left problem -> do
      IO.hPutStrLn IO.stderr ("ERROR: " <> problem)
      Exit.exitFailure
    Right config -> pure config

  contents <- case Config.input config of
    Nothing -> ByteString.getContents
    Just filePath -> ByteString.readFile filePath
  case Config.output config of
    Nothing -> ByteString.putStr contents
    Just filePath -> ByteString.writeFile filePath contents

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
