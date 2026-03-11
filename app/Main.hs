module Main (main) where

import Lib
import Data.Complex

main :: IO ()
main = do
  let qc = qstate 3
  let part = [h [0..2], x [1], cz [0, 1] [2], x [1], h [0..2], x [0..2], cz [0, 1] [2], x [0..2]]
  let program = (part ++ part) ++ [h [0..2]]
  print (getProb "101" (calc qc program))
