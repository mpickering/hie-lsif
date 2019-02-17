module Main where

import qualified LoadHIE as HIE
import qualified WriteJSON as HIE

fn = "file:///home/matt/hie-lsif/test/simple-tests/A.hs"
mac_fn = "file:///Users/matt/hie-lsif/test/simple-tests/A.js"
fn2 = "file:///home/matt/hie-lsif/test/simple-tests/A.js"

root = "/Users/matt/hie-lsif/test/simple-tests/"
root_groups = "/Users/matt/hie-lsif/groups-0.4.1.0/"


main :: IO ()
main = do
  fns <- HIE.getHieFilesIn "test/hie-files/"
--  refs1 <- HIE.collectAllReferences ["test/simple-tests/A.hie"
--                                    ,"test/simple-tests/B.hie"]
  refs1 <- HIE.collectAllReferences fns
  HIE.writeJSON root_groups refs1
