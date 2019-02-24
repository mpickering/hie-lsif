module Main where

import qualified WriteLSIF as LSIF

fn = "file:///home/matt/hie-lsif/test/simple-tests/A.hs"
mac_fn = "file:///Users/matt/hie-lsif/test/simple-tests/A.js"
fn2 = "file:///home/matt/hie-lsif/test/simple-tests/A.js"

root = "/Users/matt/hie-lsif/test/simple-tests/"
root_groups = "/Users/matt/hie-lsif/groups-0.4.1.0/"

main :: IO ()
main = LSIF.generateFromDir root_groups "test/hie-files/"
