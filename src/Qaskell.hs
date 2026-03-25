module Qaskell
    (
    Qc,
    Qstate,
    displayState,
    displayProb,
    qc,
    qstate,
    state,
    toArr,
    run,
    meas,
    prob,
    getBit,
    getProb,
    getProgram,
    x,
    y,
    z,
    h,
    g,
    cx,
    cy,
    cz,
    ch,
    cg,
    groverOp,
    getOracle,
    toBin,
    ) where

import Data.Complex
import System.Random
import Debug.Trace


data Qc = Qc [(Complex Float, Complex Float)] deriving Show
data Qstate = Qstate [Complex Float] deriving Show

qc :: Int -> (Qc, Int)
qc n = (Qc (take (n) (repeat ((1:+0::Complex Float, 0:+0::Complex Float)))), n)

qstate :: Int -> (Qstate, Int, String)
qstate n = state (qc n)

toArr :: (Qstate, Int, String) -> [Complex Float]
toArr ((Qstate q), _, _) = q

meas :: (Qstate, Int, String) -> String
meas (q, n, _) = map (\x -> if x == '0' then '1' else '0') (reverse (toBin (calcProb (prob (q, n)) (random (mkStdGen 1) :: (Float, StdGen))) n))

calcProb :: [Float] -> (Float, StdGen) -> Int
calcProb r (n, _) = getOutput r n

getOutput :: [Float] -> Float -> Int
getOutput (r:rs) n =  if n > r then (getOutput rs (n-r)) + 1 else 0

getProgram :: (Qstate, Int, String) -> String
getProgram (_, _, s) = take ((length s) - 2) (s)

displayState :: (Qstate, Int, String) -> IO ()
displayState ((Qstate q), n, _) = do
  putStrLn (getString q n)

displayProb :: (Qstate, Int, String) -> IO ()
displayProb (q, n, _) = do
  putStrLn (getString (prob (q, n)) n)

getString :: (Show a) => [a] -> Int -> String
getString [x] n = (reverse (toBin 0 n)) ++ ": " ++ (show x)
getString (x:xs) n = (reverse (toBin (length xs) n)) ++ ": " ++ (show x) ++ "\n" ++ (getString xs n)

toBin :: Int -> Int -> String
toBin x 1 = if (mod x 2) == 1 then "0" else "1"
toBin x n = (if (mod x 2) == 1 then "0" else "1") ++ (toBin (div x 2) (n-1))

run :: [(Qstate, Int, String) -> (Qstate, Int, String)] -> (Qstate, Int, String) -> (Qstate, Int, String)
run gs q = doGates q (reverse gs)

doGates :: (Qstate, Int, String) -> [(Qstate, Int, String) -> (Qstate, Int, String)] -> (Qstate, Int, String)
doGates q [] = q
doGates q [g] = g q
doGates q (g:gs) = g (doGates q gs)

getOracle :: String -> [(Qstate, Int, String) -> (Qstate, Int, String)]
getOracle s = do
  let mark = x (calcMark s)
  let num = (length s) - 1
  [mark] ++ [cz [0..(num-1)] [num]] ++ [mark]

calcMark :: String -> [Int]
calcMark ['0'] = [0]
calcMark ['1'] = []
calcMark ('0':s) = (length s) : (calcMark s)
calcMark ('1':s) = calcMark s

groverOp :: Int -> [(Qstate, Int, String) -> (Qstate, Int, String)] -> (Qstate, Int, String) -> (Qstate, Int, String)
groverOp amt o (q, n, s) = do
  let num = n-1
  let gate = [h [0..num], x [0..num], cz [0..(num-1)] [num], x [0..num], h [0..num]]
  let program = [h [0..num]] ++ repeatList amt (o++gate)
  run program (q, n, s)

repeatList :: Int -> [a] -> [a]
repeatList 1 l = l
repeatList n l = l ++ repeatList (n-1) l

x :: [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
x n (q, a, s) = appGate n (tensorPow (length n) [[0, 1], [1, 0]]) (s ++ "x " ++ (show n) ++ ", ") (q, a, s)

y :: [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
y n (q, a, s) = appGate n (tensorPow (length n) [[0, 0 :+ 1], [0 :+ (-1), 0]]) (s ++ "y " ++ (show n) ++ ", ") (q, a, s)

z :: [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
z n (q, a, s) = appGate n (tensorPow (length n) [[1, 0], [0, (-1)]]) (s ++ "z " ++ (show n) ++ ", ") (q, a, s)

h :: [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
h n (q, a, s) = do
  let mag = (1/(2**0.5))
  let m = [map (*mag) [1, 1], map (*mag) [1, (-1)]]
  appGate n (tensorPow (length n) m) (s ++ "h " ++ (show n) ++ ", ") (q, a, s)

g :: [[Complex Float]] -> [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
g m n (q, a, s) = appGate n (tensorPow (length n) m) (s ++ "g " ++ (show m) ++ " " ++ (show n) ++ ", ") (q, a, s)

cx :: [Int] -> [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
cx c t (q, a, s) = do
  let (qt, at) = mcmt [[0, 1], [1, 0]] c t (q, a)
  (qt, at, (s ++ "cx " ++ (show c) ++ " " ++ (show t) ++ ", "))

cy :: [Int] -> [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
cy c t (q, a, s) = do
  let (qt, at) = mcmt [[0, 0 :+ 1], [0 :+ (-1), 0]] c t (q, a)
  (qt, at, (s ++ "cy " ++ (show c) ++ " " ++ (show t) ++ ", "))

cz :: [Int] -> [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
cz c t (q, a, s) = do
  let (qt, at) = mcmt [[1, 0], [0, (-1)]] c t (q, a)
  (qt, at, (s ++ "cz " ++ (show c) ++ " " ++ (show t) ++ ", "))

ch :: [Int] -> [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
ch c t (q, a, s) = do
  let mag = (1/(2**0.5))
  let m = [map (*mag) [1, 1], map (*mag) [1, (-1)]]
  let (qt, at) = mcmt m c t (q, a)
  (qt, at, (s ++ "ch " ++ (show c) ++ " " ++ (show t) ++ ", "))

cg :: [[Complex Float]] -> [Int] -> [Int] -> (Qstate, Int, String) -> (Qstate, Int, String)
cg m c t (q, a, s) = do
  let (qt, at) = mcmt m c t (q, a)
  (qt, at, (s ++ "cg " ++ (show m) ++ " " ++ (show c) ++ " " ++ (show t) ++ ", "))

mcmt :: [[Complex Float]] -> [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
mcmt m c [t] q = mcg m c [t] q
mcmt m c (t:ts) q = mcg m c [t] (mcmt m c ts q)

mcg :: [[Complex Float]] -> [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
mcg g c t (q, a) = do
  let (qt, at, _) = appGate (c++t) (genMat g (length c)) "" (q, a, "")
  (qt, at)

genMat :: [[Complex Float]] -> Int -> [[Complex Float]]
genMat m cn =  extMat (getControl (2^((cn+1)-1) - 1)) m ((2^(cn+1)) - 2) 1

targetMat :: Int -> [[Complex Float]] -> [[Complex Float]]
targetMat 1 m = m
targetMat n m = extMat m (targetMat (n-1) m) (length m) ((length m)*n)

getControl :: Int -> [[Complex Float]]
getControl 1 = [[1, 0], [0, 1]]
getControl n = extMat [[1, 0], [0, 1]] (getControl (n-1)) 2 (2*n)

extMat :: [[Complex Float]] -> [[Complex Float]] -> Int -> Int -> [[Complex Float]]
extMat x y a b = (padDown b x) ++ (padUp a y)

repeatMat :: Int -> Int -> [[Complex Float]] -> [[Complex Float]]
repeatMat 1 l m = (padDown l m) ++ (padUp l m)
repeatMat n l m = (padDown l m) ++ (padUp l (repeatMat (n-1) l m))

padDown :: Int -> [[Complex Float]] -> [[Complex Float]]
padDown n m = map (++(take n (repeat (0 :+ 0)))) m

padUp :: Int -> [[Complex Float]] -> [[Complex Float]]
padUp n m = map ((take n (repeat (0 :+ 0)))++) m

appGate :: [Int] -> [[Complex Float]] -> String -> (Qstate, Int, String) -> (Qstate, Int, String)
appGate n m s ((Qstate q), qn, _) = do
  let l = generateGroups n qn
  let arr = map (\x -> map (q!!) x) l
  let oarr = map (\x -> v_mMult x m) arr
  let oq = reorder (unwrap l) (unwrap oarr)
  ((Qstate oq), qn, s)

reorder :: [Int] -> [Complex Float] -> [Complex Float]
reorder [n] [a] = appVal [] n a
reorder (n:ns) (a:as) = appVal (reorder ns as) n a

appVal :: [Complex Float] -> Int -> Complex Float -> [Complex Float]
appVal [] 0 a = [a]
appVal [] n a = (0 :+ 0) : appVal [] (n-1) a
appVal (x:xs) 0 a = a:xs
appVal (x:xs) n a = x:(appVal xs (n-1) a)

generateGroups :: [Int] -> Int -> [[Int]]
generateGroups n qn = do 
  let li = map (\x -> 2^x) n
  let num = 2^qn
  groups li num

groups :: [Int] -> Int -> [[Int]]
groups li num = do
  let amt = 2 ^ (length li)
  loopGroups (num - amt) amt li 0 [False]

loopGroups :: Int -> Int -> [Int] -> Int -> [Bool] -> [[Int]]
loopGroups 0 amt li n b = do
  let (g, bs) = getGroup li n b
  [g]
loopGroups num amt li n b = do
  let (g, bs) = getGroup li n b
  let (next, rb) = getNext n bs
  g : loopGroups (num - amt) amt li next rb

getGroup :: [Int] -> Int -> [Bool] -> ([Int], [Bool])
getGroup li n b = do
  let g = group (reverse li) n
  let bs = getBools (map (\x -> x-n) g) b
  (g, bs)

group :: [Int] -> Int -> [Int]
group [l] n = [n, (n+l)]
group (l:li) n = (group li n) ++ (group li (n+l))

getBools :: [Int] -> [Bool] -> [Bool]
getBools [g] b = appIndex g b
getBools (g:gs) b = getBools gs (appIndex g b)

appIndex :: Int -> [Bool] -> [Bool]
appIndex 0 [] = [True]
appIndex 0 (b:bs) = True:bs
appIndex n [] = False : (appIndex (n-1) [])
appIndex n (b:bs) = b : (appIndex (n-1) bs)

getNext :: Int -> [Bool] -> (Int, [Bool])
getNext n [False] = (n, [False])
getNext n [True] = (n+1, [False])
getNext n (False:bs) = (n, (False:bs))
getNext n (True:bs) = getNext (n+1) bs

unwrap :: [[a]] -> [a]
unwrap [x] = x
unwrap (x:xs) = x ++ (unwrap xs)

state :: (Qc, Int) -> (Qstate, Int, String)
state ((Qc q), n) = (Qstate (getState q), n, "")

getState :: [(Complex Float, Complex Float)] -> [Complex Float]
getState [(a, b)] = a : [b]
getState ((a, b):qs) = tensor (a : [b]) (getState qs)

tensor :: [Complex Float] -> [Complex Float] -> [Complex Float]
tensor a b = calcTensor b a a

calcTensor :: [Complex Float] -> [Complex Float] -> [Complex Float] -> [Complex Float]
calcTensor [a] [b] _ = [a*b]
calcTensor (a:as) [b] v = a*b : calcTensor as v v
calcTensor (a:as) (b:bs) v = a*b : calcTensor (a:as) bs v

tensorPow :: Int -> [[Complex Float]] -> [[Complex Float]]
tensorPow 1 m = m
tensorPow p m = m_mTensor m (tensorPow (p-1) m)

m_mTensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
m_mTensor [x] y = v_mTensor x y
m_mTensor (x:xs) y = (v_mTensor x y) ++ (m_mTensor xs y)

v_mTensor :: [Complex Float] -> [[Complex Float]] -> [[Complex Float]]
v_mTensor [x] y = s_mMult x y
v_mTensor (x:xs) y = addMat (s_mMult x y) (v_mTensor xs y)

s_mMult :: Complex Float -> [[Complex Float]] -> [[Complex Float]]
s_mMult s m = map (\x -> map (\x -> s*x) x) m

addMat :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
addMat [x] [y] = [x++y]
addMat (x:xs) (y:ys) = (x++y) : (addMat xs ys)

prob :: (Qstate, Int) -> [Float]
prob ((Qstate q), _) = map (\x -> (magnitude x) ^ (2::Integer)) q

getBit :: String -> (Qstate, Int) -> Complex Float
getBit s ((Qstate q), _) = getIndex (parseBin (reverse s)) q

getProb :: String -> (Qstate, Int) -> Float
getProb s q = (realPart (getBit s q)) ^ 2

parseBin :: String -> Int
parseBin ('0':xs) = 0 + 2 * (parseBin xs)
parseBin ('1':xs) = 1 + 2 * (parseBin xs)
parseBin [] = 0

getQBit :: Int -> Qc -> (Complex Float, Complex Float)
getQBit n (Qc q) = getIndex n q

getIndex :: Int -> [b] -> b
getIndex _ [x] = x
getIndex 0 (x:_) = x
getIndex n (_:xs) = getIndex (n-1) xs

v_mMult :: (Num a) => [a] -> [[a]] -> [a]
v_mMult x [v] = [v_vMult x v]
v_mMult x (v:vs) = (v_vMult x v) : (v_mMult x vs)

v_vMult :: (Num a) => [a] -> [a] -> a
v_vMult (a:as) [b] = a*b
v_vMult [a] (b:bs) = a*b
v_vMult (a:as) (b:bs) = a*b + (v_vMult as bs)
