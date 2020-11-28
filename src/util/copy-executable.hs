module Main ( main ) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Process as Process

main :: IO ()
main = do
  raw <- Process.readProcess "cabal" [ "exec", "which", "zippy" ] ""
  let path = List.dropWhileEnd Char.isSpace raw
  print path
  Directory.copyFile path "output/zippy"
