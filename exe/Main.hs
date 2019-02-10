module Main where

import qualified LoadHIE as HIE
import qualified WriteJSON as HIE

fn = "file:///home/matt/hie-lsif/test/simple-tests/A.js"

main :: IO ()
main = HIE.collectAllReferences ["test/simple-tests/A.hie"] >>= HIE.writeJSON fn
