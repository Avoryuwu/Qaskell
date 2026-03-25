module Main (main) where

import Qaskell
import Qaskell.Display

main :: IO ()
main = do
  let qnum = 4
  let grover = [run [h [0..qnum-1], x [0..(qnum-1)], cz [0..(qnum-2)] [qnum-1], x [0..(qnum-1)], h [0..qnum-1]]]
  let orac = [run (getOracle "1011")]
  let program = [h[0..(qnum-1)]] ++ (cycle (orac++grover))
  startDisplay qnum (program) (qstate qnum)
