module Main where

import qualified LoadHIE as HIE
import qualified WriteJSON as HIE

fn = "file:///home/matt/hie-lsif/test/simple-tests/A.hs"
mac_fn = "file:///Users/matt/hie-lsif/test/simple-tests/A.js"
fn2 = "file:///home/matt/hie-lsif/test/simple-tests/A.js"


main :: IO ()
main = do
  refs1 <- HIE.collectAllReferences ["test/simple-tests/A.hie"
                                    ,"test/simple-tests/B.hie"]
  HIE.writeJSON mac_fn refs1
