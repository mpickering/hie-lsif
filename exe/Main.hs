module Main where

import qualified WriteLSIF as LSIF
import System.Directory
import System.Environment

defaultOpts :: IO LSIF.Opts
defaultOpts = do
  cwd <- getCurrentDirectory
  return $ LSIF.Opts cwd cwd False

parseOpts :: [String] -> LSIF.Opts -> LSIF.Opts
parseOpts ("--root":dir:xs) o = (parseOpts xs o) { LSIF.root_dir = dir }
parseOpts ("--hie":dir:xs) o = (parseOpts xs o) { LSIF.hie_dir = dir }
parseOpts ("--include-contents":xs) o = (parseOpts xs o) { LSIF.include_contents = True }
parseOpts xs o = o

main :: IO ()
main = do
  args <- getArgs
  opts <- defaultOpts
  LSIF.generateFromDir (parseOpts args opts)
