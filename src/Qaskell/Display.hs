module Qaskell.Display (startDisplay) where

import Qaskell
import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Debug.Trace

startDisplay :: Int -> [(Qstate, Int) -> (Qstate, Int)] -> (Qstate, Int) -> IO ()
startDisplay qnum prog initialState = do
  let initial = map (\x -> (magnitude x, ((phase x) + pi)/(2*pi)*360, x)) (toArr initialState)
  play
    (InWindow "Nice Window" (1920, 1080) (10, 10))
    white
    60
    (0, initial, initial)
    (getPicture qnum)
    (doEvents prog qnum initial)
    easing

doEvents :: [(Qstate, Int) -> (Qstate, Int)] -> Int -> [(Float, Float, Complex Float)] -> Event -> (Int, [(Float, Float, Complex Float)], [(Float, Float, Complex Float)]) -> (Int, [(Float, Float, Complex Float)], [(Float, Float, Complex Float)])
doEvents prog qnum _ (EventKey (SpecialKey KeyEnter) Down _ _) (n, a, _) = (n+1, a, program prog qnum (n+1))
doEvents _ _ initial (EventKey (SpecialKey KeyDelete) Down _ _) (n, a, _) = (0, a, initial)
doEvents _ _ _ e t = t

program :: [(Qstate, Int) -> (Qstate, Int)] -> Int -> Int -> [(Float, Float, Complex Float)]
program prog qnum n = do
  let qs = qstate (qnum)
  map (\x -> (magnitude x, ((phase x) + pi)/(2*pi)*360, x)) (toArr (run (take n prog) qs))

getPicture :: Int -> (Int, [(Float, Float, Complex Float)], [(Float, Float, Complex Float)]) -> Picture
getPicture qnum w = do
  let l = line [(-960, -400), (960, -400)]
  let s = pictures (showState qnum (1600/(fromIntegral (2^(qnum)))) (2^(qnum)) w)
  pictures [l, s]

showState :: Int -> Float -> Int -> (Int, [(Float, Float, Complex Float)], [(Float, Float, Complex Float)]) -> [Picture]
showState qnum a 1 (i, ((m, p, c):xs), _) = [pictures [
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
showState qnum a n (i, ((m, p, c):xs), b) = pictures [
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
  ] : (showState qnum a (n-1) (i, xs, b))


getColor :: Float -> Color
getColor p = getRGB (hsv (360 - p) 0.7 1)

getRGB :: RGB Float -> Color
getRGB (RGB {channelRed = r, channelGreen = g, channelBlue = b}) = makeColor r g b 1

easing :: Float -> (Int, [(Float, Float, Complex Float)], [(Float, Float, Complex Float)]) -> (Int, [(Float, Float, Complex Float)], [(Float, Float, Complex Float)])
easing t (n, a, b) = (n, ease t a b, b)

ease :: Float -> [(Float, Float, Complex Float)] -> [(Float, Float, Complex Float)] -> [(Float, Float, Complex Float)]
ease t [] [] = []
ease t ((m, p, c):as) ((tm, tp, tc):bs) = (((1-(tanh (t*10)))*m + (tanh (t*10))*tm), ((1-(tanh (t*10)))*p + (tanh (t*10))*tp), tc):(ease t as bs)

