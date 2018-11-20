module WorldStepper (worldStepper) where

import Lib
import Data.Maybe
import Data.List
import Debug.Trace

interval :: Float
interval = 0.5

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y:(merge (x:xs) ys)
  | otherwise = x:(merge xs (y:ys))

--Assumes both lists sorted
moveToForeground :: FallingBlock -> Fg -> Fg
moveToForeground fb@(FallingBlock tetra _ _) (Fg ts) =
  Fg $ merge (zip (blockPoints fb) (repeat tetra)) ts

fallBlock :: FallingBlock -> FallingBlock
fallBlock (FallingBlock tet (y, x) rot) = FallingBlock tet (y+1, x) rot

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu rs) = Menu menu rs
worldStepper dt game
  | (accTime game) + dt < (interval / (acceleration game)) = game { accTime = (accTime game) + dt }
  | any (\((y,_), _) -> y == 0) (unFg . for $ game)= Menu (M 2) (rands game)
  | otherwise = game { for = for'''
                     , fall = chosenBlock
                     , accTime = 0
                     , rands   = rands' }
  where
    (Play for' opacity' mines' wtf' fall' _ (r:rands') accTime' _ _) = game

    fall'' = fallBlock fall'
    collided = hasCollided for' (fall'')

    chosenBlock :: FallingBlock
    chosenBlock = if collided
                    then newFallingBlock r -- Make new falling block
                    else fall''

    for'' = if collided
               then moveToForeground fall' for'
               else for'
    for''' = removeFullRows for''

    -- Minesweeper stuff

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- This sucks, I'm so sorry
index :: [[(a, b)]] -> [[((Int, Int), a, b)]]
index xss = zipWith f [0..] (map (zip [0..]) xss)
  where
    f :: Int -> [(Int, (a, b))] -> [((Int, Int), a, b)]
    f y xts = map (\(x, (a, b)) -> ((y, x), a, b)) xts


-- getMines :: Background -> [(Int, Int)]
-- getMines (Background bss) = map fst3 (concat (map (filter isMine) (index bss)))
--   where
--     isMine :: ((Int, Int), Maybe Char, Int) -> Bool
--     isMine t = isJust (snd3 t)

touchingMine :: Mines -> FallingBlock -> Bool
touchingMine (Mines ms) fb = any ((flip elem) (map fst ms)) (blockPoints fb)

encircled :: Game -> Game
encircled (Play {mines = m, for = f})@g = g {mines = vM,foundChars = vC} where
  (vM,vC) = adjustBasedOnBool v1 (unMines m)
  adjustBasedOnBool :: [Bool] -> [((Int,Int),Char)] -> ([((Int,Int),Char)],[Char])
  adjustBasedOnBool [] xs = (xs,[])
  adjustBasedOnBool xs [] = ([],[])
  adjustBasedOnBool (True:bs)  (((y,x),c):xs) = threaderMine ((y,x),c) (adjustBasedOnBool bs xs)
  adjustBasedOnBool (False:bs) (((y,x),c):xs) = threaderChar (c) (adjustBasedOnBool bs xs)
  threaderChar :: Char -> ([((Int,Int),Char)],[Char]) -> ([((Int,Int),Char)],[Char])
  threaderChar ks (bs,cs) = (bs,ks:cs)
  threaderMine :: ((Int,Int),Char) -> ([((Int,Int),Char)],[Char]) -> ([((Int,Int),Char)],[Char])
  threaderMine a (as,bs) = (a:as,bs)
  v1 = fmap ((testWith (((fmap fst) . unFg) f)) . fst) (fmap ((\(x,_) -> ([(x,Nothing)])) . fst) (unMines m))
  testWith :: [(Int,Int)] -> ([((Int, Int),Maybe Rotation)]) -> Bool
  testWith [] gs = False
  testWith fs [] = False
  testWith fs gs = hastermed v2 || testWith fs v2
  v2 = (remove fs ((remdups . expand) gs))
  hastermed :: [((Int, Int),Maybe Rotation)] -> Bool
  hastermed = any testEdge
  testEdge :: ((Int,Int),Maybe Rotation) -> Bool
  testEdge ((y,x),_) = y < 0 || x < 0 || y >= worldHeight || x >= worldWidth
  expand :: [((Int, Int),Maybe Rotation)] -> [((Int, Int),Maybe Rotation)]
  expand [] = []
  expand (((y,x),Nothing):xs) = [((y-1,x),Just North),((y,x-1),Just East),((y+1,x),Just South),((y,x+1),Just West)] ++ (expand xs)
  expand (((y,x),(Just North)):xs) = [((y-1,x),Just North),((y,x-1),Just East),((y,x+1),Just West)] ++ (expand xs)
  expand (((y,x),(Just South)):xs) = [((y,x-1),Just East),((y+1,x),Just South),((y,x+1),Just West)] ++ (expand xs)
  expand (((y,x),(Just East)):xs) = [((y-1,x),Just North),((y,x-1),Just East),((y+1,x),Just South)] ++ (expand xs)
  expand (((y,x),(Just West)):xs) = [((y-1,x),Just North),((y+1,x),Just South),((y,x+1),Just West)] ++ (expand xs)
  remdups :: [((Int, Int),Maybe Rotation)] -> [((Int, Int),Maybe Rotation)]
  remdups = (fmap head) . (groupBy g) . (sortBy f)
  f :: ((Int, Int),Maybe Rotation) -> ((Int, Int),Maybe Rotation) -> Ordering
  f ((a,_),_) ((b,_),_) = compare a b
  g :: ((Int, Int),Maybe Rotation) -> ((Int, Int),Maybe Rotation) -> Bool
  g ((y1,x1),_) ((y2,x2),_) = (y2 == y1) && (x2 == x1)
  remove :: [(Int,Int)] -> ([((Int, Int),Maybe Rotation)]) -> ([((Int, Int),Maybe Rotation)])
  remove [] xs = xs
  remove (y:ys) xs = remove ys (removeValue y xs)
  removeValue :: (Int,Int) -> ([((Int, Int),Maybe Rotation)]) -> ([((Int, Int),Maybe Rotation)])
  removeValue _ [] = []
  removeValue (y1,x1) (((y2,x2),dir):xs) | y1 == x1 && y2 == x2 = removeValue (y1,x1) xs
                                         | otherwise            = ((y2,x2),dir) : (removeValue (y1,x1) xs)
encircled (Menu a b) = Menu a b

removeFullRows :: Fg -> Fg
removeFullRows (Fg xs) = Fg (fst . foldr f' ([],0) $ ys)  where
  ys = groupBy (f (==)) . sortBy (f compare) $ xs
  f :: (Int -> Int -> b) -> ((Int,Int),Tetramino) -> ((Int,Int),Tetramino) -> b
  f g ((a,_),_) ((b,_),_) = g a b
  f' :: [((Int,Int),Tetramino)] -> ([((Int,Int),Tetramino)], Int) -> ([((Int,Int),Tetramino)], Int)
  f' ps (ts, offset)
    | length ps /= worldWidth = (map (\((y, x), t) -> ((y + offset, x), t)) ps ++ ts, offset)
    | otherwise = (ts, offset + 1)
