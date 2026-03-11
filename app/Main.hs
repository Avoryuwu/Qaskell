module Main (main) where

import Lib
import Data.Complex

main :: IO ()
main = do
  let qc = qstate 3
  let program = [groverOp 2 (getOracle "101")]
  displayProb (calc program qc)
