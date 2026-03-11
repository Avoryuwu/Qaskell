module Lib
    (
    Qc,
    Qstate,
    displayState,
    displayProb,
    qc,
    qstate,
    state,
    calc,
    prob,
    getBit,
    getProb,
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
    ) where

import Data.Complex


data Qc = Qc [(Complex Float, Complex Float)] deriving Show
data Qstate = Qstate [Complex Float] deriving Show

qc :: Int -> (Qc, Int)
qc n = (Qc (take (n) (repeat ((1:+0::Complex Float, 0:+0::Complex Float)))), n)

qstate :: Int -> (Qstate, Int)
qstate n = state (qc n)

displayState :: (Qstate, Int) -> IO ()
displayState ((Qstate q), n) = do
  putStrLn (getString q n)

displayProb :: (Qstate, Int) -> IO ()
displayProb (q, n) = do
  putStrLn (getString (prob (q, n)) n)

getString :: (Show a) => [a] -> Int -> String
getString [x] n = (toBin 0 n) ++ ": " ++ (show x)
getString (x:xs) n = (toBin (length xs) n) ++ ": " ++ (show x) ++ "\n" ++ (getString xs n)

toBin :: Int -> Int -> String
toBin x 1 = if (mod x 2) == 1 then "0" else "1"
toBin x n = (if (mod x 2) == 1 then "0" else "1") ++ (toBin (div x 2) (n-1))


calc :: [(Qstate, Int) -> (Qstate, Int)] -> (Qstate, Int) -> (Qstate, Int)
calc gs q = doGates q (reverse gs)

doGates :: (Qstate, Int) -> [(Qstate, Int) -> (Qstate, Int)] -> (Qstate, Int)
doGates q [g] = g q
doGates q (g:gs) = g (doGates q gs)

getOracle :: String -> [(Qstate, Int) -> (Qstate, Int)]
getOracle s = do 
  let mark = x (calcMark s)
  let num = (length s) - 1
  ([mark] ++ [cz [0..(num-1)] [num]]) ++ [mark]

calcMark :: String -> [Int]
calcMark ['0'] = [0]
calcMark ['1'] = []
calcMark ('0':s) = (length s) : (calcMark s)
calcMark ('1':s) = calcMark s

groverOp :: Int -> [(Qstate, Int) -> (Qstate, Int)] -> (Qstate, Int) -> (Qstate, Int)
groverOp amt o (q, n) = do
  let num = n-1
  let gate = [h [0..num], x [0..num], cz [0..(num-1)] [num], x [0..num], h [0..num]]
  let program = [h [0..num]] ++ repeatList amt (o++gate)
  calc program (q, n)

repeatList :: Int -> [a] -> [a]
repeatList 1 l = l
repeatList n l = l ++ repeatList (n-1) l

x :: [Int] -> (Qstate, Int) -> (Qstate, Int)
x n q = appGate n (tensorPow (length n) [[0, 1], [1, 0]]) q

y :: [Int] -> (Qstate, Int) -> (Qstate, Int)
y n q = appGate n (tensorPow (length n) [[0, 0 :+ 1], [0 :+ (-1), 0]]) q

z :: [Int] -> (Qstate, Int) -> (Qstate, Int)
z n q = appGate n (tensorPow (length n) [[1, 0], [0, (-1)]]) q

h :: [Int] -> (Qstate, Int) -> (Qstate, Int)
h n q = do
  let mag = (1/(2**0.5))
  let m = [map (*mag) [1, 1], map (*mag) [1, (-1)]]
  appGate n (tensorPow (length n) m) q

g :: [[Complex Float]] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
g m n q = appGate n (tensorPow (length n) m) q

cx :: [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
cx = mcmt [[0, 1], [1, 0]]

cy :: [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
cy = mcmt [[0, 0 :+ 1], [0 :+ (-1), 0]]

cz :: [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
cz = mcmt [[1, 0], [0, (-1)]]

ch :: [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
ch = do
  let mag = (1/(2**0.5))
  let m = [map (*mag) [1, 1], map (*mag) [1, (-1)]]
  mcmt m

cg :: [[Complex Float]] -> [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
cg m = mcmt m

mcmt :: [[Complex Float]] -> [Int] -> [Int] -> (Qstate, Int) -> (Qstate, Int)
mcmt m c [t] q = mcg m c [t] q
mcmt m c (t:ts) q = mcg m c [t] (mcmt m c ts q)

mcg :: [[Complex Float]] -> [Int] -> [Int] -> (Qstate, Int) -> (Qstate,Int)
mcg g c t q = appGate (c++t) (genMat g (length c)) q

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

appGate :: [Int] -> [[Complex Float]] -> (Qstate, Int) -> (Qstate, Int)
appGate n m ((Qstate q), qn) = do
  let l = generateGroups n qn
  let arr = map (\x -> map (q!!) x) l
  let oarr = map (\x -> v_mMult x m) arr
  let oq = reorder (unwrap l) (unwrap oarr)
  ((Qstate oq), qn)

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

state :: (Qc, Int) -> (Qstate, Int)
state ((Qc q), n) = (Qstate (getState q), n)

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
