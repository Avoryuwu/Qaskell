module Qaskell.Display (startDisplay) where

import Qaskell
import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Debug.Trace

data World = World (Int, String, [Int], [(Float, Float, Complex Float)], [(Float, Float, Complex Float)])

startDisplay :: Int -> [(Qstate, Int, String) -> (Qstate, Int, String)] -> (Qstate, Int, String) -> IO ()
startDisplay qnum p initialState = do
  let initial = map (\x -> (magnitude x, ((phase x) + pi)/(2*pi)*360, x)) (toArr initialState)
  let prog = take 10000 p
  play
    (InWindow "Nice Window" (1920, 1080) (10, 10))
    white
    60
    (World (0, "", [], initial, initial))
    (getPicture qnum (length prog))
    (doEvents prog qnum initial)
    easing

doEvents :: [(Qstate, Int, String) -> (Qstate, Int, String)] -> Int -> [(Float, Float, Complex Float)] -> Event -> World -> World
doEvents prog qnum _ (EventKey (SpecialKey KeyRight) Down _ _) (World (n, s, i, a, p)) = if n == length prog then World (n, s, i, a, p) else do
  let (st, pt) = program prog qnum (n+1)
  let l = length st
  World (n+1, drop (if n == 0 then 0 else i!!(n-1)) st, if n == length i then i++[l+2] else i, a, pt)
doEvents prog qnum initial (EventKey (SpecialKey KeyLeft) Down _ _) (World (n, s, i, a, _)) = if n == 0 then World (0, "", [], a, initial) else do
  let (st, pt) = program prog qnum (n-1)
  World (n-1, drop (if n <= 3 then 0 else i!!(n-3)) st, i, a, pt)
doEvents _ _ initial (EventKey (SpecialKey KeyDelete) Down _ _) (World (n, _, _, a, _)) = World (0, "", [], a, initial)
doEvents _ _ _ _ t = t

program :: [(Qstate, Int, String) -> (Qstate, Int, String)] -> Int -> Int -> (String, [(Float, Float, Complex Float)])
program prog qnum n = do
  let qs = qstate (qnum)
  let q = run (take n prog) qs
  (getProgram q, map (\x -> (magnitude x, ((phase x) + pi)/(2*pi)*360, x)) (toArr q))

getPicture :: Int -> Int -> World -> Picture
getPicture qnum pnum w = do
  let l = line [(-960, -400), (960, -400)]
  let (World (n, str, _, _, _)) = w
  let num = translate (-920) (500) (scale 0.15 0.15 (text ("step " ++ (show n) ++ " of " ++ (show pnum) ++ ": " ++ str)))
  let s = pictures (showState qnum (1600/(fromIntegral (2^(qnum)))) (2^(qnum)) w)
  pictures [l, s, num]

showState :: Int -> Float -> Int -> World -> [Picture]
showState qnum a 1 (World (i, _, _, ((m, p, c):xs), _)) = [pictures [
  (translate
    ((960 - a/4) - (a*(2^qnum))/8)
    (300*m-400)
    (color
      (getColor p)
      (rectangleSolid
        (a/2)
        (600*m)))),
  (translate
    ((960 - a/4) - (a*(2^qnum))/8 - 5)
    (600*m-250)
    (rotate 90 (scale 0.1 0.1 (text (show c))))),
  (translate
    ((960 - a/4) - (a*(2^qnum))/8 - 5)
    (-410)
    (rotate 90 (scale 0.15 0.15 (text (toBin 0 qnum)))))]
  ]
showState qnum a n (World (i, s, is, ((m, p, c):xs), b)) = pictures [
  (translate 
    (((960) - (a*((fromIntegral n)-1)) - a/4) - (a*(2^qnum))/8)
    (300*m-400)
    (color
      (getColor p)
      (rectangleSolid
        (a/2)
       (600*m)))),
  (translate 
    (((960) - (a*((fromIntegral n)-1)) - a/4) - (a*(2^qnum))/8 - 5)
    (600*m-250)
    (rotate 90 (scale 0.1 0.1 (text (show c))))),
  (translate
    (((960) - (a*((fromIntegral n)-1)) - a/4) - (a*(2^qnum))/8 - 5)
    (-410)
    (rotate 90 (scale 0.15 0.15 (text (reverse (toBin (n-1) qnum))))))
  ] : (showState qnum a (n-1) (World (i, s, is, xs, b)))


getColor :: Float -> Color
getColor p = getRGB (hsv (360 - p) 0.7 1)

getRGB :: RGB Float -> Color
getRGB (RGB {channelRed = r, channelGreen = g, channelBlue = b}) = makeColor r g b 1

easing :: Float -> World -> World
easing t (World (n, s, i, a, b)) = World (n, s, i, ease t a b, b)

ease :: Float -> [(Float, Float, Complex Float)] -> [(Float, Float, Complex Float)] -> [(Float, Float, Complex Float)]
ease t [] [] = []
ease t ((m, p, c):as) ((tm, tp, tc):bs) = (((1-(tanh (t*10)))*m + (tanh (t*10))*tm), ((1-(tanh (t*10)))*p + (tanh (t*10))*tp), tc):(ease t as bs)

