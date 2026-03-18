module Main (main) where

import Qaskell

main :: IO ()
main = do
  let qc = qstate 3
  let program = [groverOp 2 (getOracle "110")]
  displayState (run program qc)
